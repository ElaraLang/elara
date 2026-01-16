{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use id" #-}

module Elara.Query.TH (makeTag) where

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
makeTag :: Name -> TH.Q [TH.Dec]
makeTag tyName = do
    info <- reify tyName
    cons <- case info of
        TH.TyConI (TH.DataD _ _ _ _ cs _) -> pure cs
        TH.TyConI (TH.NewtypeD _ _ _ _ c _) -> pure [c]
        _ -> fail "makeTag: expected data/newtype"
    let names = concatMap toNames cons
        mkPat (n, ar) = conP n (replicate ar wildP)
        mkClause i na = clause [mkPat na] (normalB (litE (IntegerL (fromIntegral i)))) []
    sig <- sigD (mkName "tagQuery") [t|forall es a. $(conT tyName) es a -> Int|]
    fun <- funD (mkName "tagQuery") (zipWith mkClause [0 ..] names)
    pure [sig, fun]
