{-# LANGUAGE PatternSynonyms #-}

{- |
Maranget-style pattern match compilation using a compact pattern matrix.

This module normalises surface patterns into 'NPat' and compiles a matrix
of such patterns into Core case expressions. We store the rectangular grid
of patterns in a 'Data.Matrix.Matrix', while per-row metadata (RHS payloads
and accumulated variable bindings) are kept in parallel lists aligned by
row index.

Key points:
- We always discriminate on column 0 (the head scrutinee) and recursively
  refine the matrix.
- Variable patterns behave as wildcards for decision making, but we record
  their binds and emit them as nested non-recursive Core lets at leaves.
- Constructor alternatives extend the scrutinee list with freshly bound
  field variables and push their argument patterns into the matrix.
-}
module Elara.ToCore.Match where

import Data.Map.Strict qualified as M
import Data.Matrix qualified as Mat
import Data.Text qualified as T
import Elara.AST.Generic.Types qualified as AST
import Elara.AST.Name (NameLike (..), Qualified, VarName)
import Elara.AST.Region
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (UnlocatedVarRef, pattern UnlocatedLocal)
import Elara.Core qualified as Core
import Elara.Core.Generic qualified as G
import Elara.Data.Unique (Unique)
import Polysemy.State ()

{- Our initial step in compiling pattern matching is to construct a pattern matrix
of the patterns used in a match
For example, consider the following code:
@
f l = match l with
    (Nil, _) -> x
    (_, Nil) -> y
    (Cons x xs, Cons y ys) -> z
@

The ultimate goal with this desugaring process is to turn all pattern matching into simple matches upon either:
- A single value
- A constructor with its values bound as variables

For example, the above code would be desugared into something like:
@
f l = match l with
    (x, y) -> match x with
        Nil -> x
        _ -> match y with
            Nil -> y
            _ -> z
@

however this is difficult to do in a single pass.

Basic pattern-matrix construction

We define a small normalized pattern language (NPat) that abstracts over the
surface TypedPattern forms but keeps exactly the information a matrix needs:

- Wildcards/variables as default-able cases (we record var names for later binds)
- Literals as equality-discriminated heads
- Constructors with their subpatterns (still in NPat form). We keep the
  constructor as a qualified Text (no DataCon resolution here) to avoid
  coupling and cycles; resolution can happen later when building Core.

For now we build a single-column matrix since `match` in the current AST
matches one scrutinee. Product (tuple/record) expansion can be layered on later
by transforming the first column.
-}

-- | Normalized pattern
data NPat
    = PWild
    | PVar (Unique VarName) -- variable bind; treated like wildcard for the decision tree
    | PLit Core.Literal -- literal match
    | PCon (Qualified Text) [NPat] -- constructor with subpatterns
    deriving (Show, Eq)

{- | Pattern matrix backed by 'Data.Matrix'. Invariant:
 nrows pmPats == length pmRhs == length pmBinds
-}
data PMatrix a = PMatrix
    { pmPats :: Mat.Matrix NPat
    , pmRhs :: [a]
    , pmBinds :: [[(Unique VarName, Core.Var)]]
    }
    deriving (Show, Eq)

{- | Build a single-column pattern matrix from branches (pattern, rhs).
The RHS is left polymorphic so callers can carry either TypedExpr or CoreExpr.
-}
buildMatrix1 :: [(TypedPattern, a)] -> PMatrix a
buildMatrix1 branches =
    let pats = Mat.fromList (length branches) 1 (map (toNPat . fst) branches)
        rhs = map snd branches
        binds = replicate (length branches) []
     in PMatrix{pmPats = pats, pmRhs = rhs, pmBinds = binds}

-- | Convert a TypedPattern into our normalized NPat form.
toNPat :: TypedPattern -> NPat
toNPat (AST.Pattern (Located _ pat, _t)) = go pat
  where
    go = \case
        AST.IntegerPattern i -> PLit (Core.Int i)
        AST.FloatPattern f -> PLit (Core.Double f)
        AST.StringPattern s -> PLit (Core.String s)
        AST.CharPattern c -> PLit (Core.Char c)
        AST.UnitPattern -> PLit Core.Unit
        AST.WildcardPattern -> PWild
        AST.VarPattern (Located _ uvn) -> PVar uvn
        AST.ConstructorPattern (Located _ qn) ps ->
            PCon (fmap nameText qn) (map toNPat ps)

-- No explicit partition/drop helpers are needed with the matrix-based layout.

{-
  Minimal compiler from a PMatrix to Core.
  - Single scrutinee to start; after matching a constructor with n fields we
    extend the scrutinee list with the freshly-bound field variables before the
    remaining columns. We always discriminate on column 0.
  - Variable patterns are currently treated as wildcards; binding them to user
    names can be layered in later by threading a bind-env per row and emitting
    Let at leaves.
-}

type ConResolver m = Qualified Text -> m Core.DataCon

type FreshLocal m = T.Text -> Core.Type -> m Core.Var

{- | Compile a pattern matrix to a Core expression. Assumes the first column
corresponds to the head scrutinee in the provided list of variables.
-}
compileMatrix :: Monad m => ConResolver m -> FreshLocal m -> [Core.Var] -> PMatrix Core.CoreExpr -> m Core.CoreExpr
compileMatrix resolveCon fresh scruts pm =
    if Mat.ncols pm.pmPats == 0
        then case (pm.pmRhs, pm.pmBinds) of
            (rhs0 : _, binds0 : _) -> pure (emitBinds binds0 rhs0)
            _ -> pure (Core.Lit Core.Unit)
        else stepOnColumn0 resolveCon fresh scruts pm

stepOnColumn0 :: Monad m => ConResolver m -> FreshLocal m -> [Core.Var] -> PMatrix Core.CoreExpr -> m Core.CoreExpr
stepOnColumn0 resolveCon fresh scruts pm = do
    case scruts of
        [] ->
            -- No scrutinee left. If matrix is solved, emit first row; else Unit.
            if Mat.ncols pm.pmPats == 0
                then case (pm.pmRhs, pm.pmBinds) of
                    (rhs0 : _, binds0 : _) -> pure (emitBinds binds0 rhs0)
                    _ -> pure (Core.Lit Core.Unit)
                else pure (Core.Lit Core.Unit)
        s0 : restScruts -> do
            let rows = Mat.nrows pm.pmPats
                cols = Mat.ncols pm.pmPats
                rowInfo i =
                    if cols == 0
                        then Nothing
                        else
                            let p0 = Mat.getElem i 1 pm.pmPats
                                ps = [Mat.getElem i j pm.pmPats | j <- [2 .. cols]]
                             in Just (p0, ps)
                foldPart i (consM, litM, defs) =
                    case rowInfo i of
                        Nothing -> (consM, litM, defs)
                        Just (p0, ps) -> case p0 of
                            PCon qn args ->
                                let entry = (i, args, ps)
                                 in (M.insertWith (++) qn [entry] consM, litM, defs)
                            PLit l ->
                                let entry = (i, ps)
                                 in (consM, M.insertWith (++) l [entry] litM, defs)
                            PWild -> (consM, litM, (i, p0, ps) : defs)
                            PVar _ -> (consM, litM, (i, p0, ps) : defs)
                (consM, litM, defs) = foldr foldPart (M.empty, M.empty, []) [1 .. rows]

            if not (M.null consM)
                then do
                    conAlts <- forM (M.toList consM) $ \(qn, matched) -> do
                        dc <- resolveCon qn
                        let argTys = Core.functionTypeArgs (Core.dataConType dc)
                        xs <- forM (zip [0 ..] argTys) $ \(i, ty) -> fresh ("p" <> show i) ty
                        let n = length xs
                            transformMatched (i, args, tailPs) =
                                let (args', addBinds) = bindImmediate xs args
                                    newRow = args' ++ tailPs
                                    rhsRow = at1 i pm.pmRhs
                                    bindsRow = addBinds ++ at1 i pm.pmBinds
                                 in (newRow, rhsRow, bindsRow)
                            transformDefault (i, headPat, tailPs) =
                                let pad = replicate n PWild
                                    newRow = pad ++ tailPs
                                    rhsRow = at1 i pm.pmRhs
                                    bindsRow = case headPat of
                                        PVar u -> (u, s0) : at1 i pm.pmBinds
                                        _ -> at1 i pm.pmBinds
                                 in (newRow, rhsRow, bindsRow)
                            rowsForAlt = map transformMatched matched ++ map transformDefault defs
                        body <-
                            if null rowsForAlt
                                then pure (Core.Lit Core.Unit)
                                else do
                                    let newPats = Mat.fromLists (map (\(r, _, _) -> r) rowsForAlt)
                                        newRhs = map (\(_, r, _) -> r) rowsForAlt
                                        newBs = map (\(_, _, b) -> b) rowsForAlt
                                    compileMatrix resolveCon fresh (xs ++ restScruts) PMatrix{pmPats = newPats, pmRhs = newRhs, pmBinds = newBs}
                        pure (Core.DataAlt dc, xs, body)
                    defAlt <- compileDefault resolveCon fresh s0 restScruts pm defs
                    pure $ Core.Match (Core.Var s0) (Just s0) (conAlts ++ maybeToList defAlt)
                else
                    if not (M.null litM)
                        then do
                            litAlts <- forM (M.toList litM) $ \(l, matched) -> do
                                let transformMatched (i, tailPs) =
                                        let rhsRow = at1 i pm.pmRhs
                                            bindsRow = at1 i pm.pmBinds
                                         in (tailPs, rhsRow, bindsRow)
                                    transformDefault (i, headPat, tailPs) =
                                        let rhsRow = at1 i pm.pmRhs
                                            bindsRow = case headPat of
                                                PVar u -> (u, s0) : at1 i pm.pmBinds
                                                _ -> at1 i pm.pmBinds
                                         in (tailPs, rhsRow, bindsRow)
                                    rowsForAlt = map transformMatched matched ++ map transformDefault defs
                                body <-
                                    if null rowsForAlt
                                        then pure (Core.Lit Core.Unit)
                                        else do
                                            let newPats = Mat.fromLists (map (\(r, _, _) -> r) rowsForAlt)
                                                newRhs = map (\(_, r, _) -> r) rowsForAlt
                                                newBs = map (\(_, _, b) -> b) rowsForAlt
                                            compileMatrix resolveCon fresh restScruts PMatrix{pmPats = newPats, pmRhs = newRhs, pmBinds = newBs}
                                pure (Core.LitAlt l, [], body)
                            defAlt <- compileDefault resolveCon fresh s0 restScruts pm defs
                            pure $ Core.Match (Core.Var s0) (Just s0) (litAlts ++ maybeToList defAlt)
                        else do
                            -- Only defaults (wild/var). Drop head and continue.
                            let transformDefault (i, headPat, tailPs) =
                                    let rhsRow = at1 i pm.pmRhs
                                        bindsRow = case headPat of
                                            PVar u -> (u, s0) : at1 i pm.pmBinds
                                            _ -> at1 i pm.pmBinds
                                     in (tailPs, rhsRow, bindsRow)
                                rowsOnlyDef = map transformDefault defs
                            if null rowsOnlyDef
                                then pure (Core.Lit Core.Unit)
                                else do
                                    let newPats = Mat.fromLists (map (\(r, _, _) -> r) rowsOnlyDef)
                                        newRhs = map (\(_, r, _) -> r) rowsOnlyDef
                                        newBs = map (\(_, _, b) -> b) rowsOnlyDef
                                    compileMatrix resolveCon fresh restScruts PMatrix{pmPats = newPats, pmRhs = newRhs, pmBinds = newBs}
  where
    -- Bind immediate constructor-argument PVars to the provided xs.
    bindImmediate :: [Core.Var] -> [NPat] -> ([NPat], [(Unique VarName, Core.Var)])
    bindImmediate xs args =
        let step (x, p) = case p of
                PVar u -> (PWild, Just (u, x))
                other -> (other, Nothing)
            (args', newBinds) = unzip (zipWith (curry step) xs args)
         in (args', catMaybes newBinds)

{- | Build a DEFAULT alternative when default rows exist.
 'defs' contains triples of (rowIndex, headPat, tailPatterns)
-}
compileDefault ::
    Monad m =>
    ConResolver m ->
    FreshLocal m ->
    Core.Var ->
    [Core.Var] ->
    PMatrix Core.CoreExpr ->
    [(Int, NPat, [NPat])] ->
    m (Maybe Core.CoreAlt)
compileDefault resolveCon fresh s0 restScruts pm defs =
    case defs of
        [] -> pure Nothing
        ds -> do
            let rows' =
                    [ ( tailPs
                      , at1 i pm.pmRhs
                      , case headPat of
                            PVar u -> (u, s0) : at1 i pm.pmBinds
                            _ -> at1 i pm.pmBinds
                      )
                    | (i, headPat, tailPs) <- ds
                    ]
                newPats = Mat.fromLists (map (\(r, _, _) -> r) rows')
                newRhs = map (\(_, r, _) -> r) rows'
                newBs = map (\(_, _, b) -> b) rows'
            body <-
                if Mat.nrows newPats == 0
                    then pure (Core.Lit Core.Unit)
                    else compileMatrix resolveCon fresh restScruts PMatrix{pmPats = newPats, pmRhs = newRhs, pmBinds = newBs}
            pure (Just (Core.DEFAULT, [], body))

-- Emit nested non-recursive lets for collected variable bindings.
emitBinds :: [(Unique VarName, Core.Var)] -> Core.CoreExpr -> Core.CoreExpr
emitBinds [] body = body
emitBinds bs body =
    let mkBinder :: (Unique VarName, Core.Var) -> Maybe Core.Var
        mkBinder (u, v) = case v of
            Core.Id _ ty _ ->
                let uqText = fmap nameText u
                    b :: UnlocatedVarRef Text
                    b = UnlocatedLocal uqText
                 in Just (Core.Id b ty Nothing)
            Core.TyVar _ -> Nothing
        mkLet (b, v) = Core.Let (G.NonRecursive (b, Core.Var v))
        pairs = [(b, v) | p <- bs, let (_, v) = p, Just b <- [mkBinder p]]
     in foldr mkLet body pairs

-- 1-based indexing helper aligned with Data.Matrix row indices.
at1 :: Int -> [a] -> a
at1 i xs = case drop (i - 1) xs of
    (y : _) -> y
    [] -> error "PMatrix row index out of bounds"
