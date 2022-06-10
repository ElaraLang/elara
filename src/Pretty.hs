module Pretty where

import Data.Map qualified as M
import Data.Text qualified as Text (Text, unpack)
import Elara.Name (Name)
import Elara.Name qualified as Name
import Text.PrettyPrint
import TypeInfer.Type qualified as T
import Prelude hiding ((<>))

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Text.Text where
  ppr _ = text . Text.unpack

instance Pretty Name where
  ppr _ x = text $ Name.stringValue x

instance Pretty T.TVar where
  ppr i (T.TV x) = ppr i x

instance Pretty T.Type where
  ppr p (T.TFunc a b) = parensIf (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
    where
      isArrow (T.TFunc _ _) = True
      isArrow _ = False
  ppr p (T.TApp a b) = ppr p a <+> ppr p b
  ppr p (T.TVariable a) = ppr p a
  ppr p (T.TCon a) = ppr p a

instance Pretty T.Scheme where
  ppr p (T.Forall [] t) = ppr p t
  ppr p (T.Forall ts t) = text "forall" <+> hcat (punctuate space (map (ppr p) ts)) <> text "." <+> ppr p t

instance (Pretty a) => Pretty [a] where
  ppr p = hcat . map (ppr p)

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  ppr p = vcat . map (\(k, v) -> ppr p k <+> text "=" <+> ppr p v) . M.toList

prettyPrint :: (Pretty a) => a -> Doc
prettyPrint = ppr 0