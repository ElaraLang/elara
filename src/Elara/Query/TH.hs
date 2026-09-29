{-# LANGUAGE TemplateHaskell #-}

-- | Derives the tagging, comparison, hashing and key-checking boilerplate for 'Elara.Query.Query'
module Elara.Query.TH (makeTag, deriveSameCtor, deriveHashableInstance, deriveKeyChecks) where

import Data.Data (eqT, typeRep, (:~:) (Refl))
import Data.GADT.Compare (GOrdering (..))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype (applySubstitution, freeVariables, tvName)

import Data.Map qualified as Map

import Elara.Query.Generics (checkKey)

-- | A query constructor, normalised from whichever 'Con' shape 'reify' gave us
data QueryCon = QueryCon
    { conName :: Name
    , conFields :: [Type]
    , conTypeArgs :: [Name]
    -- ^ Existential variables with a 'Typeable' constraint, bound by type application in patterns
    , conEqualities :: Map Name Type
    -- ^ Equality constraints like @loc ~ SourceRegion@, used to make field types concrete
    }

queryCons :: Name -> Q [QueryCon]
queryCons ty =
    reify ty >>= \case
        TyConI (DataD _ _ _ _ cons _) -> traverse (queryCon [] []) cons
        info -> fail ("Expected a data type, got " <> show info)

queryCon :: [Name] -> Cxt -> Con -> Q QueryCon
queryCon vars cxt = \case
    ForallC vars' cxt' con -> queryCon (vars <> map tvName vars') (cxt <> cxt') con
    GadtC [n] fields _ -> pure (mk n (map snd fields))
    RecGadtC [n] fields _ -> pure (mk n (map (\(_, _, t) -> t) fields))
    NormalC n fields -> pure (mk n (map snd fields))
    RecC n fields -> pure (mk n (map (\(_, _, t) -> t) fields))
    InfixC (_, l) n (_, r) -> pure (mk n [l, r])
    con -> fail ("Unsupported constructor: " <> show con)
  where
    mk n fields =
        QueryCon
            { conName = n
            , conFields = fields
            , conTypeArgs = filter (\v -> any (isTypeable v) cxt) vars
            , conEqualities = Map.fromList (mapMaybe equality cxt)
            }

    isTypeable v = \case
        AppT (ConT c) (VarT v') -> c == ''Typeable && v' == v
        _ -> False

    equality = \case
        AppT (AppT EqualityT (VarT v)) t -> Just (v, t)
        AppT (AppT (ConT eq) (VarT v)) t | nameBase eq == "~" -> Just (v, t)
        _ -> Nothing

conPat :: QueryCon -> [Name] -> [Name] -> Pat
conPat con tys xs = ConP (conName con) (map VarT tys) (map VarP xs)

newNames :: String -> [a] -> Q [Name]
newNames prefix = traverse (const (newName prefix))

-- | Generates @tagQuery :: forall es a. Query es a -> Int@, numbering constructors in declaration order
makeTag :: Name -> Q [Dec]
makeTag ty = do
    cons <- queryCons ty
    let name = mkName ("tag" <> nameBase ty)
        clause i con = Clause [ConP (conName con) [] (WildP <$ conFields con)] (NormalB (LitE (IntegerL i))) []
    sig <- sigD name [t|forall es a. $(conT ty) es a -> Int|]
    pure [sig, FunD name (zipWith clause [0 ..] cons)]

-- | Generates @sameCtor :: Query es a -> Query es b -> GOrdering a b@ for two queries with the same constructor
deriveSameCtor :: Name -> Q [Dec]
deriveSameCtor ty = do
    cons <- queryCons ty
    let name = mkName "sameCtor"
    sig <- sigD name [t|forall es a b. HasCallStack => $(conT ty) es a -> $(conT ty) es b -> GOrdering a b|]
    clauses <- traverse sameCtorClause cons
    q1 <- newName "q1"
    q2 <- newName "q2"
    mismatch <- [|error ("sameCtor called with mismatched constructors: " <> show $(varE q1) <> " and " <> show $(varE q2))|]
    pure [sig, FunD name (clauses <> [Clause [VarP q1, VarP q2] (NormalB mismatch) []])]

sameCtorClause :: QueryCon -> Q Clause
sameCtorClause con = do
    ls <- newNames "l" (conFields con)
    rs <- newNames "r" (conFields con)
    tls <- newNames "tL" (conTypeArgs con)
    trs <- newNames "tR" (conTypeArgs con)
    body <- compareTypes tls trs (compareFields ls rs [|GEQ|])
    pure (Clause [conPat con tls ls, conPat con trs rs] (NormalB body) [])

compareFields :: [Name] -> [Name] -> Q Exp -> Q Exp
compareFields (l : ls) (r : rs) equal =
    [|
        case compare $(varE l) $(varE r) of
            LT -> GLT
            GT -> GGT
            EQ -> $(compareFields ls rs equal)
        |]
compareFields _ _ equal = equal

-- | Compares type arguments first, so that 'Refl' brings the field types into agreement before 'compareFields'
compareTypes :: [Name] -> [Name] -> Q Exp -> Q Exp
compareTypes (l : ls) (r : rs) equal =
    [|
        case eqT @($(varT l)) @($(varT r)) of
            Just Refl -> $(compareTypes ls rs equal)
            Nothing
                | typeRep (Proxy @($(varT l))) < typeRep (Proxy @($(varT r))) -> GLT
                | otherwise -> GGT
        |]
compareTypes _ _ equal = equal

-- | Generates @instance Hashable (Query es a)@, hashing the constructor tag, then fields, then type arguments
deriveHashableInstance :: Name -> Q [Dec]
deriveHashableInstance ty = do
    cons <- queryCons ty
    salt <- newName "salt"
    q <- newName "q"
    es <- newName "es"
    a <- newName "a"
    matches <- zipWithM (hashMatch salt) [0 ..] cons
    let method = FunD 'hashWithSalt [Clause [VarP salt, VarP q] (NormalB (CaseE (VarE q) matches)) []]
    pure [InstanceD Nothing [] (ConT ''Hashable `AppT` (ConT ty `AppT` VarT es `AppT` VarT a)) [method]]

hashMatch :: Name -> Integer -> QueryCon -> Q Match
hashMatch salt i con = do
    xs <- newNames "x" (conFields con)
    tys <- newNames "t" (conTypeArgs con)
    let tagged = [|hashWithSalt $(varE salt) ($(litE (integerL i)) :: Int)|]
        withFields = foldl' (\acc x -> [|hashWithSalt $acc $(varE x)|]) tagged xs
        withTypes = foldl' (\acc t -> [|hashWithSalt $acc (typeRep (Proxy @($(varT t))))|]) withFields tys
    body <- withTypes
    pure (Match (conPat con tys xs) (NormalB body) [])

{- | Generates one @_ = checkKey \@"Con" \@n \@FieldType@ binding per constructor field,
so a location-bearing query key fails to compile with an error naming the query and field
-}
deriveKeyChecks :: Name -> Q [Dec]
deriveKeyChecks ty = do
    cons <- queryCons ty
    concat <$> traverse keyChecks cons

keyChecks :: QueryCon -> Q [Dec]
keyChecks con = zipWithM check [1 ..] (conFields con)
  where
    check :: Integer -> Type -> Q Dec
    check i field = do
        let field' = applySubstitution (conEqualities con) field
        unless (null (freeVariables field')) $
            fail (nameBase (conName con) <> " field " <> show i <> " has a type that can't be made concrete: " <> pprint field')
        body <- [|checkKey @($(litT (strTyLit (nameBase (conName con))))) @($(litT (numTyLit i))) @($(pure field'))|]
        pure (ValD WildP (NormalB body) [])
