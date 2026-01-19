{-# LANGUAGE UndecidableInstances #-}

{- |
= Pattern Match Compilation (Maranget's Algorithm)

This module compiles high-level pattern matching
into low-level Core @match@ expressions which only contain a single branch at once.

== The Pattern Matrix
We treat the match expressions as a matrix.
Given (in pseudo-Elara):
@
match x, y with
    Just a, Right b -> 1
    _, Left c -> 2
@

The initial matrix is constructed as:
@
       Col 0      Col 1
Row 1: [ Just a ] [ Right b ] => Body 1
Row 2: [ _      ] [ Left c  ] => Body 2
@
with scrutinees: @[x, y]@

== The Algorithm Step
We look at **Column 0** (the head) to decide what to do with variable @x@.
1. We see a constructor @Just@. We must generate a @match x with ...@.
2. We create a branch for @Just@. Inside this branch, @x@ is unwrapped into @payload@.
   We transform the matrix for this branch ("Specialization"):
   - Row 1 matches @Just a@. The pattern @Just a@ becomes @a@.
   - Row 2 is a wildcard @_@. It matches @Just@ too! We expand @_@ into @wildcard_payload@.

   New Matrix for "Just" branch:
   @
          New Col    Old Col 1
   Row 1: [ a      ] [ Right b ]
   Row 2: [ _      ] [ Left c  ]
   @
   New Scrutinees: [payload, y]

3. We recurse until the matrix is empty.
-}
module Elara.ToCore.Match (buildMatrix1, compileMatrix) where

import Data.Map.Strict qualified as M
import Data.Matrix qualified as Mat
import Data.Text (toLower)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Elara.AST.Generic.Types qualified as AST
import Elara.AST.Name (NameLike (..), Qualified (..), TypeName (..), VarName)
import Elara.AST.Region
import Elara.AST.Typed as Typed
import Elara.AST.VarRef (UnlocatedVarRef, VarRef' (..))
import Elara.Core qualified as Core
import Elara.Core.Analysis qualified as Core
import Elara.Core.Generic qualified as G
import Elara.Core.Pretty ()
import Elara.Data.Pretty
import Elara.Data.Unique (Unique)
import Elara.Logging

-- | Normalized pattern representation for the matrix
data NPat
    = -- | wildcard pattern
      PWild
    | -- | variable pattern that behaves like a wildcard but binds to a name
      PVar (Unique VarName)
    | -- | Literal pattern
      PLit Core.Literal
    | -- | constructor with subpatterns
      PCon (Qualified TypeName) [NPat]
    deriving (Show, Eq, Generic)

instance Pretty NPat

{- | The Pattern Matrix, parametrized over the type of the RHS expressions.
Invariant: @nrows pmPats == length pmRhs == length pmBinds@
-}
data PMatrix a = PMatrix
    { pmPats :: Mat.Matrix NPat
    {- ^ The grid of patterns.
    Rows correspond to match alternatives, columns to scrutinees.
    -}
    , pmRhs :: [a]
    {- ^ The right hand side (body) associated with each row.
    when a row is fully matched (i.e. all the columns are cleared), we emit this expression.
    -}
    , pmBinds :: [[(Unique VarName, Core.Var)]]
    {- ^ Accumulated variable bindings for each row.
    As we traverse the matrix we encounter 'PVar' patterns which bind variables.
    Rather than immediately emitting @let@ bindings, we accumulate them here.
    If row N ends up matching, we emit @'pmRhs' !! N@ wrapped in @let@ bindings for all @'pmBinds' !! N@.
    -}
    }
    deriving (Show, Eq)

{- | A row view of the matrix used for convenient iteration.
Tuple contains:
The list of patterns in this row (corresponding to the current columns)
The RHS for this row
The accumulated bindings for this row
-}
type RichRow a = ([NPat], a, [(Unique VarName, Core.Var)])

{- | Initialise a matrix from a list of match arms.
This matrix typically starts with a single column (the top-level pattern matches).
As we decompose constructors, the matrix will grow wider.
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
            PCon qn (map toNPat ps)

-- | A function that resolves a constructor name to its DataCon.
type ConResolver m = Qualified TypeName -> m Core.DataCon

-- | A function that generates a fresh local variable given a base name and type.
type FreshLocal m = T.Text -> Core.Type -> m Core.Var

{- | Compile a pattern matrix to a Core expression.

Assumes the first column of the matrix corresponds to the first variable in the 'scruts' list.
This function is recursive:
1. If the matrix is empty (width 0), we are done, so we emit the RHS of the first valid row.
2. If we have patterns but no scrutinees, we emit a unit value (which indicates a non-exhaustive match).
3. Otherwise, we inspect the first column ('processHeadColumn') to decide how to branch, and recurse.
-}
compileMatrix ::
    StructuredDebug :> r =>
    -- | Function to resolve constructor names
    ConResolver (Eff r) ->
    -- | Function to generate fresh local variables
    FreshLocal (Eff r) ->
    -- | The scrutinee variables
    [Core.Var] ->
    -- | The pattern matrix
    PMatrix Core.CoreExpr ->
    -- | The compiled Core expression
    Eff r Core.CoreExpr
compileMatrix resolveCon fresh scruts pm
    | Mat.ncols pm.pmPats == 0 -- base case: no more columns, so all patterns matched
        =
        case (pm.pmRhs, pm.pmBinds) of -- pick the first row that matches
            (rhs0 : _, binds0 : _) -> pure (emitBinds binds0 rhs0)
            _ -> pure (Core.Lit Core.Unit) -- this implies the match is not exhaustive. should probably error?
            -- No scrutinees left (shouldn't happen if cols > 0).
    | null scruts = pure (Core.Lit Core.Unit)
    -- Recursively decompose the matrix based on Column 0.
    | otherwise = processHeadColumn resolveCon fresh scruts pm

{- | The core logic: Examine Column 0 and determine the branching strategy.
We extract Column 0 and split the matrix rows into groups:
1. Rows matching specific Constructors.
2. Rows matching specific Literals.
3. "Default" rows (Wildcards/Vars) that match everything.
-}
processHeadColumn ::
    StructuredDebug :> r =>
    -- | Function to resolve constructor names
    ConResolver (Eff r) ->
    -- | Function to generate fresh local variables
    FreshLocal (Eff r) ->
    -- | The scrutinee variables
    [Core.Var] ->
    -- | The pattern matrix
    PMatrix Core.CoreExpr ->
    -- | The compiled Core expression
    Eff r Core.CoreExpr
processHeadColumn resolveCon fresh (s0 : restScruts) pm = do
    -- Explode Matrix into RichRows for safe iteration without index partiality
    let rows :: [RichRow Core.CoreExpr] = zip3 (Mat.toLists pm.pmPats) pm.pmRhs pm.pmBinds

    -- partition the rows based on the head pattern in each row
    -- conMap = mapping constructors to the rows that match them
    -- litMap = mapping literals to the rows that match them
    -- defaults = rows that are wildcards/vars at head
    let (conMap, litMap, defaults) = partitionRows rows

    if not (M.null conMap)
        then -- we have some constructor cases to compile, so we have to emit a match over s0
            compileConstructorCases resolveCon fresh s0 restScruts conMap defaults
        else
            if not (M.null litMap)
                then -- we have some literal cases to compile, so we have to emit a match over s0
                    compileLiteralCases resolveCon fresh s0 restScruts litMap defaults
                else do
                    -- the only remaining cases are defaults (wildcards/vars)
                    -- therefore we don't need any branching, and we can just "consume" s0
                    -- we effectively treat s0 as matched by all rows, and remove it from the scrutinee list
                    let newRows = map (expandDefaultRow 0 s0) defaults
                    compileMatrix resolveCon fresh restScruts (rebuildMatrix newRows)
processHeadColumn _ _ [] _ = error "processHeadColumn: No scrutinee available"

{- | Compile a match on Constructors (e.g. @match x with { Just y -> ... } @).

For every constructor @C@ that appears in the column:
1. Generate a @Case C@ branch.
2. Inside that branch, @x@ is unwrapped into fields @f1, f2...@.
3. Construct a new sub-matrix for this branch:
   - For rows matching @C f1 f2@, we replace @C f1 f2@ with @f1, f2@.
   - For default rows @_@, we expand @_@ into @_ _@ (one wild for each field).
4. Recurse on the sub-matrix with new scrutinees @[f1, f2, ...rest]@.
-}
compileConstructorCases ::
    StructuredDebug :> r =>
    -- | Function to resolve constructor names
    ConResolver (Eff r) ->
    -- | Function to generate fresh local variables
    FreshLocal (Eff r) ->
    -- | The scrutinee variable for this column, e.g. @x@
    Core.Var ->
    -- | The remaining scrutinee variables that correspond to later columns
    [Core.Var] ->
    -- | Map of constructor names to the rows that match them
    M.Map (Qualified TypeName) [RichRow Core.CoreExpr] ->
    -- | The default rows (wildcards/vars)
    [RichRow Core.CoreExpr] ->
    Eff r Core.CoreExpr
compileConstructorCases resolveCon fresh s0 restScruts conMap defaults = do
    conAlts <- forM (M.toList conMap) $ \(conName, matchedRows) -> do
        dc <- resolveCon conName

        -- first we need to figure out the types of the constructor fields
        -- to determine the types of the fields being unwrapped
        -- as DataCon can be polymorphic, we have to perform type substitution based on the scrutinee type

        -- the type of the scrutinee variable
        let scrutType = Core.varType s0
        -- the arguments that the scrutinee's type constructor was applied to
        let scrutTypeArgs = Core.conTyArgs scrutType
        -- then we split the DataCon type into its forall'd type variables and the core type
        let (dcTyVars, dcTau) = Core.splitForAlls (Core.dataConType dc)

        -- now we create a substitution from the DataCon's type variables to the scrutinee's type arguments
        let subst = M.fromList (zip dcTyVars scrutTypeArgs)
        -- and apply that substitution to each argument type of the DataCon
        let dcArgTysRaw = Core.functionTypeArgs dcTau
        -- this gives us the real types of the constructor fields in this context
        let realArgTys = map (Core.substType subst) dcArgTysRaw

        -- now we can create a fresh local variable for each of the constructor fields
        fieldVars <- forM (zip [0 ..] realArgTys) $ do
            let namePrefix = case conName of
                    Qualified (TypeName t) _ -> toLower t <> "_field_"
            \(i, ty) -> fresh (namePrefix <> show i) ty

        -- build the new matrix for this constructor branch
        let expandedDefaults = map (expandDefaultRow (length fieldVars) s0) defaults
        let allRows = matchedRows <> expandedDefaults

        -- recurse on the new matrix with the new scrutinees (the constructor fields + the remaining scrutinees)
        body <- compileMatrix resolveCon fresh (fieldVars <> restScruts) (rebuildMatrix allRows)
        pure (Core.DataAlt dc, fieldVars, body)

    defAlt <- compileDefaultBranch resolveCon fresh s0 restScruts defaults
    pure $ Core.Match (Core.Var s0) (Just s0) (conAlts ++ maybeToList defAlt)

-- | Compile a match on Literals (e.g. @match x with { 1 -> ... } @).
compileLiteralCases ::
    StructuredDebug :> r =>
    ConResolver (Eff r) ->
    FreshLocal (Eff r) ->
    -- | The scrutinee variable for this column, e.g. @x@
    Core.Var ->
    -- | The remaining scrutinee variables that correspond to later columns
    [Core.Var] ->
    -- | Map of literals to the rows that match them
    M.Map Core.Literal [RichRow Core.CoreExpr] ->
    -- | The default rows (wildcards/vars)
    [RichRow Core.CoreExpr] ->
    Eff r Core.CoreExpr
compileLiteralCases resolveCon fresh s0 restScruts litMap defaults = do
    litAlts <- forM (M.toList litMap) $ \(lit, matchedRows) -> do
        -- literals have no fields to unwrap, so we just build the new matrix directly
        -- Matched rows have head column removed.
        -- Default rows need head column removed (arity 0 expansion).
        let expandedDefaults = map (expandDefaultRow 0 s0) defaults
        let allRows = matchedRows <> expandedDefaults

        body <- compileMatrix resolveCon fresh restScruts (rebuildMatrix allRows)
        pure (Core.LitAlt lit, [], body)

    defAlt <- compileDefaultBranch resolveCon fresh s0 restScruts defaults
    pure $ Core.Match (Core.Var s0) (Just s0) (litAlts ++ maybeToList defAlt)

{- | Compile the default branch for a @Match@ expression.
This handles rows that didn't match any specific Constructor/Literal in the other branches,
and generates a 'Core.DEFAULT' branch.
If there are no default rows, we return 'Nothing'.
-}
compileDefaultBranch ::
    StructuredDebug :> r =>
    ConResolver (Eff r) ->
    FreshLocal (Eff r) ->
    -- | The scrutinee variable for this column, e.g. @x@
    Core.Var ->
    -- | The remaining scrutinee variables that correspond to later columns
    [Core.Var] ->
    -- | The default rows (wildcards/vars)
    [RichRow Core.CoreExpr] ->
    Eff r (Maybe Core.CoreAlt)
compileDefaultBranch _ _ _ _ [] = pure Nothing
compileDefaultBranch resolveCon fresh s0 restScruts defaults = do
    -- Strip the default pattern from head
    -- If it was a PVar, record the binding to s0.
    let newRows = map (expandDefaultRow 0 s0) defaults
    body <- compileMatrix resolveCon fresh restScruts (rebuildMatrix newRows)
    pure (Just (Core.DEFAULT, [], body))

-- | Converts a list of 'RichRows' back into the 'PMatrix' structure required for recursion
rebuildMatrix :: [RichRow a] -> PMatrix a
rebuildMatrix rows =
    let (pats, rhs, binds) = unzip3 rows
     in PMatrix
            { pmPats = Mat.fromLists pats
            , pmRhs = rhs
            , pmBinds = binds
            }

-- | Partition a list of 'RichRow's into constructor cases, literal cases, and default cases.
partitionRows ::
    -- | Input rows
    [RichRow a] ->
    ( M.Map (Qualified TypeName) [RichRow a]
    , -- \^ Constructor cases
      M.Map Core.Literal [RichRow a]
    , -- \^ Literal cases
      [RichRow a]
    )
-- \^ Default cases

partitionRows = foldr go (M.empty, M.empty, [])
  where
    go row@(pats, rhs, binds) (cons, lits, defs) =
        case pats of
            [] -> (cons, lits, defs) -- Should be unreachable if cols > 0
            (headPattern : remainingPatterns) -> case headPattern of
                PCon name args ->
                    -- constructor pattern
                    -- expand the subpatterns into the row
                    -- the new column 'PCon' gets replaced by its subpatterns 'args'
                    let newRow = (args <> remainingPatterns, rhs, binds)
                     in (M.insertWith (<>) name [newRow] cons, lits, defs)
                PLit l ->
                    -- literal pattern
                    -- strip the literal
                    let newRow = (remainingPatterns, rhs, binds)
                     in (cons, M.insertWith (<>) l [newRow] lits, defs)
                PWild -> (cons, lits, row : defs) -- keep the row as-is so that the default branch can expand it later
                PVar _ -> (cons, lits, row : defs)

{- | Takes a Default row and adapts it to match a Constructor branch.

1. Records the binding if the head was 'PVar' (binds @x = s0@).
2. Expands the head into @n@ wildcards to match the arity of the constructor.
   e.g. if matching 'Just' (arity 1), `_` becomes `_` (matching the payload).
   e.g. if matching 'Either' (arity 2), `_` becomes `_ _`.
-}
expandDefaultRow ::
    Int ->
    -- | The scrutinee variable being matched against
    Core.Var ->
    -- | The input row
    RichRow a ->
    RichRow a
expandDefaultRow numFields s0 (pats, rhs, binds) =
    case pats of
        [] -> ([], rhs, binds)
        (headPat : tailPats) ->
            let newHead = replicate numFields PWild
                newBinds = case headPat of
                    PVar u -> (u, s0) : binds
                    _ -> binds
             in (newHead <> tailPats, rhs, newBinds)

{- | Emit let-bindings around a Core expression.
Bindings are emitted in reverse order (first in list is innermost let).
-}
emitBinds ::
    -- | bindings to emit
    [(Unique VarName, Core.Var)] ->
    -- | body expression
    Core.CoreExpr ->
    -- | resulting expression with lets
    Core.CoreExpr
emitBinds [] body = body
emitBinds bs body =
    let mkLet ((u, v), b) =
            let uqText = fmap nameText u
                varRef = Local uqText :: UnlocatedVarRef T.Text
                binder = Core.Id varRef (Core.varType v) Nothing
             in Core.Let (G.NonRecursive (binder, Core.Var v)) b
     in foldr (curry mkLet) body bs
