{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use id" #-}

{- | Horrible TH code to generate the ginormous functions required for tagging, hashing and comparing Queries.
A lot of this code is AI generated so don't ask me how it works :')
-}
module Elara.Query.TH (makeTag, deriveSameCtor, deriveHashableInstance) where

import Data.Data
import Data.GADT.Compare
import Language.Haskell.TH as TH

toNames :: Con -> [(Name, Int)]
toNames = \case
    NormalC n _ -> [(n, 0 :: Int)]
    RecC n fs -> [(n, length fs)]
    InfixC _ n _ -> [(n, 2)]
    ForallC _ _ c -> toNames c
    GadtC ns fs _ -> [(n, length fs) | n <- ns]
    RecGadtC ns fs _ -> [(n, length fs) | n <- ns]

-- Generate: tagQuery :: forall es a. Query es a -> Int
makeTag :: Name -> Q [Dec]
makeTag ty = do
    TyConI (DataD _ _ _ _ cons _) <- reify ty
    let tagName = mkName ("tag" ++ nameBase ty)

    let allConNames = concatMap getNames cons

    clauses <- for (zip allConNames [0 ..]) $ \(name, index) ->
        -- replicate (getArity name cons) WildP handles the fields of the constructor
        pure $
            Clause
                [ConP name [] (replicate (getArity name cons) WildP)]
                (NormalB (LitE (IntegerL index)))
                []

    -- Generate the type signature forall es a. Query es a -> Int
    let queryType = AppT (AppT (ConT ty) WildCardT) WildCardT
    let sig = SigD tagName (AppT (AppT ArrowT queryType) (ConT ''Int))

    pure [sig, FunD tagName clauses]
  where
    getNames (NormalC n _) = [n]
    getNames (RecC n _) = [n]
    getNames (InfixC _ n _) = [n]
    getNames (ForallC _ _ c) = getNames c
    getNames (GadtC ns _ _) = ns
    getNames (RecGadtC ns _ _) = ns

    -- We need to know how many fields to ignore in the pattern match
    getArity target cons = fromMaybe 0 $ listToMaybe [length f | c <- cons, (n, _, f) <- [extractConDetails' c], n == target]

extractConDetails' :: Con -> (Name, Cxt, [Type])
extractConDetails' = \case
    NormalC n fields -> (n, [], map snd fields)
    RecC n fields -> (n, [], map (\(_, _, t) -> t) fields)
    InfixC (_, t1) n (_, t2) -> (n, [], [t1, t2])
    ForallC _ cxt con' -> let (n, _, f) = extractConDetails' con' in (n, cxt, f)
    GadtC [n] fields _ -> (n, [], map snd fields)
    RecGadtC [n] fields _ -> (n, [], map (\(_, _, t) -> t) fields)
    _ -> error "Multi-name GADTC not supported in this helper"

-- | Generates 'sameCtor :: forall es a b. Query es a -> Query es b -> GOrdering a b'
deriveSameCtor :: Name -> Q [Dec]
deriveSameCtor ty = do
    -- 1. Generate the Type Signature
    -- sameCtor :: forall es a b. Query es a -> Query es b -> GOrdering a b
    sig <- sigD (mkName "sameCtor") [t|HasCallStack => forall es a b. $(conT ty) es a -> $(conT ty) es b -> GOrdering a b|]

    -- 2. Generate the Function Clauses
    TyConI (DataD _ _ _ _ cons _) <- reify ty
    clauses <- mapM genClause cons

    q1 <- newName "q1"
    q2 <- newName "q2"
    errExp <-
        [|
            error
                ( "sameCtor called with mismatched constructors: "
                    <> show $(varE q1)
                    <> " and "
                    <> show $(varE q2)
                )
            |]

    let fallbackClause = Clause [VarP q1, VarP q2] (NormalB errExp) []

    -- 3. Return both
    pure [sig, FunD (mkName "sameCtor") (clauses <> [fallbackClause])]

deriveHashableInstance :: Name -> Q [Dec]
deriveHashableInstance ty = do
    -- 1. Setup Instance Head: instance Hashable (Query es a)
    let className = mkName "Hashable"
    esVar <- newName "es"
    aVar <- newName "a"
    -- Type: Hashable (Query es a)
    let instanceType = AppT (ConT className) (AppT (AppT (ConT ty) (VarT esVar)) (VarT aVar))

    -- 2. Define hashWithSalt
    saltName <- newName "salt"
    qName <- newName "q"

    TyConI (DataD _ _ _ _ cons _) <- reify ty

    -- Generate a match for every constructor
    matches <- zipWithM (genHashMatch saltName) [0 ..] cons

    let funDec =
            FunD
                (mkName "hashWithSalt")
                [Clause [VarP saltName, VarP qName] (NormalB (CaseE (VarE qName) matches)) []]

    pure [InstanceD Nothing [] instanceType [funDec]]

genHashMatch :: Name -> Int -> Con -> Q Match
genHashMatch saltName index con = do
    (conName, _, fieldTypes) <- extractConDetails con

    -- Generate vars for fields (x1, x2...)
    fieldNames <- replicateM (length fieldTypes) (newName "x")

    -- Generate vars for Existential Type Applications (t1, t2...)
    typeVars <- getExistentialTypeVars con
    typeVarNames <- replicateM (length typeVars) (newName "t")

    -- Pattern: Con @t1 @t2 x1 x2
    let pat = ConP conName (map VarT typeVarNames) (map VarP fieldNames)

    -- RHS Construction: Chain hashWithSalt calls
    -- 1. Start with: salt `hashWithSalt` index
    let startHash = [|hashWithSalt $(varE saltName) $(litE (IntegerL (toInteger index)))|]

    -- 2. Chain fields: ... `hashWithSalt` x1 `hashWithSalt` x2
    let hashField acc name = [|hashWithSalt $acc $(varE name)|]
    let withFields = foldl' hashField startHash fieldNames

    -- 3. Chain TypeReps: ... `hashWithSalt` (typeRep (Proxy @t1))
    let hashType acc tName =
            [|hashWithSalt $acc (typeRep (Proxy :: Proxy $(varT tName)))|]

    let finalBody = foldl' hashType withFields typeVarNames
    body <- finalBody

    pure $ Match pat (NormalB body) []

genClause :: Con -> Q Clause
genClause con = do
    (conName, _cxt, fields) <- extractConDetails con

    let nFields = length fields
    lNames <- replicateM nFields (newName "l")
    rNames <- replicateM nFields (newName "r")

    typeVars <- getExistentialTypeVars con
    lTypeNames <- replicateM (length typeVars) (newName "tL")
    rTypeNames <- replicateM (length typeVars) (newName "tR")

    -- Pattern: Con @tL1 @tL2 l1 l2
    let lPat = ConP conName (VarT <$> lTypeNames) (VarP <$> lNames)
    let rPat = ConP conName (VarT <$> rTypeNames) (VarP <$> rNames)

    -- Logic: Check Types FIRST, then Check Values
    -- If types match (eqT returns Just Refl), we enter 'valueCheck'.
    -- If types differ, we return GLT/GGT based on typeRep comparison.
    let valueCheck = nestComparison lNames rNames [|GEQ|]
    body <- nestTypeComparison lTypeNames rTypeNames valueCheck

    pure $ Clause [lPat, rPat] (NormalB body) []

-- | Helper to extract constructor details
extractConDetails :: Con -> Q (Name, Cxt, [Type])
extractConDetails = \case
    NormalC n fields -> pure (n, [], map snd fields)
    RecC n fields -> pure (n, [], map (\(_, _, t) -> t) fields)
    InfixC (_, t1) n (_, t2) -> pure (n, [], [t1, t2])
    ForallC _ cxt con' -> do
        (n, _, fields) <- extractConDetails con'
        pure (n, cxt, fields)
    GadtC [n] fields _ -> pure (n, [], map snd fields)
    RecGadtC [n] fields _ -> pure (n, [], map (\(_, _, t) -> t) fields)
    c -> fail $ "Unsupported constructor type: " ++ show c

-- | Find existential type variables that have a Typeable constraint
getExistentialTypeVars :: Con -> Q [Name]
getExistentialTypeVars (ForallC vars cxt _) = do
    pure [n | KindedTV n _ _ <- vars, isTypeableConstraint n cxt]
  where
    isTypeableConstraint n = any (isTypeable n)
    isTypeable n = \case
        AppT (ConT t) (VarT v) | t == ''Typeable && v == n -> True
        _ -> False
getExistentialTypeVars _ = pure []

-- | Chains value comparisons
nestComparison :: [Name] -> [Name] -> Q Exp -> Q Exp
nestComparison [] [] final = final
nestComparison (l : ls) (r : rs) final =
    [|
        case compare $(varE l) $(varE r) of
            LT -> GLT
            GT -> GGT
            EQ -> $(nestComparison ls rs final)
        |]
nestComparison _ _ _ = fail "Mismatched field counts"

-- | Chains type comparisons
nestTypeComparison :: [Name] -> [Name] -> Q Exp -> Q Exp
nestTypeComparison [] [] success = success
nestTypeComparison (l : ls) (r : rs) success =
    [|
        case eqT @($(varT l)) @($(varT r)) of
            Just Refl -> $(nestTypeComparison ls rs success)
            Nothing ->
                case compare (typeRep (Proxy :: Proxy $(varT l))) (typeRep (Proxy :: Proxy $(varT r))) of
                    LT -> GLT
                    GT -> GGT
                    EQ -> GLT
        |]
nestTypeComparison _ _ _ = fail "Mismatched type variable counts"
