module Error.Error where
import TypeInfer.Env (TypeError)

data Error
  = SyntaxError
  | TypeError TypeError
  deriving (Show)