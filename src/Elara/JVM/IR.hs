-- | Module for JVM Intermediate Representation.
module Elara.JVM.IR where

import Elara.Data.Pretty
import Elara.Data.Unique
import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Name qualified as JVM
import JVM.Data.Abstract.Type qualified as JVM
import Prelude hiding (Op)

data Module = Module
    { moduleName :: JVM.QualifiedClassName
    -- ^ The source module name
    , moduleClasses :: [Class]
    -- ^ All classes generated from this module
    }
    deriving (Show)

data Class = Class
    { className :: JVM.QualifiedClassName
    , classSuper :: JVM.QualifiedClassName
    , classFields :: [Field]
    , classMethods :: [Method]
    , classConstructors :: [Constructor]
    }
    deriving (Show)

data Field = Field
    { fieldName :: Text
    , fieldType :: JVM.FieldType
    }
    deriving (Show)

data Method = Method
    { methodName :: Text
    , methodDesc :: JVM.MethodDescriptor
    , methodArgs :: [(Unique Text, JVM.FieldType)]
    -- ^ Argument names and types
    , methodBody :: [Block]
    -- ^ Method body as a list of basic blocks
    , methodIsStatic :: Bool
    }
    deriving (Show)

-- | A class's constructor
data Constructor = Constructor
    { constructorDesc :: JVM.MethodDescriptor
    , constructorArgs :: [(Unique Text, JVM.FieldType)]
    , constructorBody :: [Block]
    }
    deriving (Show)

-- | A basic block of JVM instructions
data Block = Block
    { blockLabel :: Unique Text -- e.g., "entry", "loop_start"
    , instrs :: [Instruction]
    }
    deriving (Show)

-- | Abstract representation of JVM instructions
data Instruction
    = -- | r1 = ...
      Assign (Unique Text) JVM.FieldType Expr
    | -- | execute expr, drop result
      ExprStmt Expr
    | -- | return x; (for object types, uses AReturn)
      Return (Maybe Expr)
    | -- | return primitive int/boolean; (uses IReturn)
      IReturn Expr
    | -- | goto label
      Jump (Unique Text)
    | -- | if (cond) goto true else goto false (cond is Elara Bool)
      JumpIf Expr (Unique Text) (Unique Text)
    | -- | if (primBool) goto true else goto false (primBool is primitive boolean)
      JumpIfPrimitiveBool Expr (Unique Text) (Unique Text)
    | {- | Set a field on 'this' (implicit 'this' access)
      e.g. "this.x = y"
      -}
      SetField JVM.QualifiedClassName Text JVM.FieldType Expr
    | {- | Call the superclass constructor (super(...))
      The Emitter will resolve the correct "invokespecial" logic.
      -}
      Super JVM.QualifiedClassName [Expr]
    deriving (Show)

data Expr
    = -- | Literal integer
      LitInt Integer
    | -- | Literal string
      LitString Text
    | -- | Literal double
      LitDouble Double
    | -- | Literal boolean (produces Elara/Prim/Bool)
      LitBool Bool
    | -- | Primitive boolean literal (produces JVM primitive boolean)
      PrimitiveLitBool Bool
    | -- | Literal char
      LitChar Char
    | LitUnit
    | -- | Local Variable reference, the variable guaranteed to be in scope
      LocalVar (Unique Text) JVM.FieldType
    | -- | Reference to 'this' in instance methods
      This JVM.FieldType
    | FieldRef JVM.QualifiedClassName Text JVM.FieldType
    | -- | Create new object with constructor args
      New JVM.QualifiedClassName [(Expr, JVM.FieldType)]
    | -- | obj.field
      GetField
        Expr
        -- | Name of the field's class
        JVM.ClassInfoType
        Text
        JVM.FieldType
    | -- | a + b
      BinaryOp BinOp Expr Expr
    | -- | !a
      UnaryOp Op Expr
    | -- | obj instanceof Type
      InstanceOf Expr JVM.FieldType
    | -- | method call with arguments
      Call CallType [Expr]
    | MakeClosure
        { closureTargetClass :: JVM.QualifiedClassName
        -- ^ The class containing the static method
        , closureTargetMethod :: Text
        -- ^ The name of the static method to eventually invoke
        , closureTarget :: JVM.MethodDescriptor
        -- ^ The static method descriptor
        , closureInterface :: JVM.QualifiedClassName
        -- ^ The functional interface we are implementing
        , capturedValues :: [(Expr, JVM.FieldType)]
        -- ^ The arguments we have SO FAR
        }
    | MakeConstructorClosure
        { ctorClosureClass :: JVM.QualifiedClassName
        -- ^ The class whose constructor we're calling
        , ctorClosureDesc :: JVM.MethodDescriptor
        -- ^ The constructor descriptor (args -> void)
        , ctorClosureInterface :: JVM.QualifiedClassName
        -- ^ The functional interface we're implementing
        , ctorCapturedValues :: [(Expr, JVM.FieldType)]
        -- ^ The arguments we have SO FAR (partial application)
        }
    | PrimOp PrimOp [Expr]
    | -- | Cast expression to a specific type
      Cast Expr JVM.FieldType
    deriving (Show)

data CallType
    = InvokeStatic JVM.QualifiedClassName Text JVM.MethodDescriptor
    | InvokeVirtual Expr JVM.QualifiedClassName Text JVM.MethodDescriptor
    | InvokeInterface
        Expr
        JVM.QualifiedClassName
        Text
        JVM.MethodDescriptor
    deriving (Show)

-- | Elara primitive operations
data PrimOp
    = UndefinedError
    | PatternMatchFailedError
    | IntAdd -- Elara.Prim.(+) : Int -> Int -> Int
    | IntSubtract -- Elara.Prim.(-) : Int -> Int -> Int
    | IntMultiply -- Elara.Prim.(*) : Int -> Int -> Int
    | IntNegate -- Elara.Prim.negate : Int -> Int
    | PrimEquals -- Elara.Prim.(==) : a -> a -> Bool
    | PrimCompare -- Elara.Prim.compare : a -> a -> Int
    | IOBind -- Elara.Prelude.>>= : IO a -> (a -> IO b) -> IO b
    | DebugWithMsg -- debugWithMsg : String -> a -> a
    | ThrowError -- Elara.Error.error : String -> a
    | Println -- Elara.Prelude.println : String -> IO ()
    | StringCons -- Elara.Prim.stringCons : Char -> String -> String
    | StringHead -- Elara.Prim.stringHead : String -> Char
    | StringIsEmpty -- Elara.Prim.stringIsEmpty : String -> Bool
    | StringTail -- Elara.Prim.stringTail : String -> String
    | ToString -- Elara.Prim.toString : a -> String
    deriving (Show, Generic)

instance Pretty PrimOp

data BinOp = Add | Subtract | Multiply | Divide | And | Or | Equals | NotEquals | LessThan | GreaterThan
    deriving (Show)

data Op = Not | Negate
    deriving (Show)

instance Pretty Module where
    pretty (Module name classes) =
        vsep $
            [ "/* Module:" <+> pretty name <+> "*/"
            , ""
            ]
                ++ map pretty classes

instance Pretty Class where
    pretty (Class name super fields methods constructors) =
        vsep
            [ "class" <+> pretty name <+> "extends" <+> pretty super <+> lbrace
            , indent 4 $ vsep (map pretty fields)
            , ""
            , indent 4 $ vsep (map pretty methods)
            , ""
            , indent 4 $ vsep (map pretty constructors)
            , rbrace
            ]

instance Pretty Field where
    pretty (Field name ftype) =
        pretty ftype <+> pretty name <> semi

instance Pretty Method where
    pretty (Method name desc args body isStatic) =
        vsep
            [ (if isStatic then "static " else "") <> pretty desc.returnDesc <+> pretty name <> tupled (map prettyArg args) <+> lbrace
            , indent 4 $ vsep (map pretty body)
            , rbrace
            ]
      where
        prettyArg (u, t) = pretty t <+> pretty u

instance Pretty Constructor where
    pretty (Constructor desc args body) =
        vsep
            [ "constructor" <+> pretty (desc.returnDesc) <> tupled (map prettyArg args) <+> lbrace
            , indent 4 $ vsep (map pretty body)
            , rbrace
            ]
      where
        prettyArg (u, t) = pretty t <+> pretty u

instance Pretty Block where
    pretty (Block label instrs) =
        vsep
            [ pretty label <> colon
            , indent 2 $ vsep (map pretty instrs)
            ]

instance Pretty Instruction where
    pretty (Assign var ty expr) =
        pretty var <+> ":" <+> pretty ty <+> "=" <+> pretty expr <> semi
    pretty (ExprStmt expr) =
        pretty expr <> semi
    pretty (Return Nothing) =
        "return" <> semi
    pretty (Return (Just expr)) =
        "return" <+> pretty expr <> semi
    pretty (IReturn expr) =
        "ireturn" <+> pretty expr <> semi
    pretty (Jump label) =
        "goto" <+> pretty label <> semi
    pretty (JumpIf cond trueLabel falseLabel) =
        "if" <+> parens (pretty cond) <+> "goto" <+> pretty trueLabel <+> "else goto" <+> pretty falseLabel <> semi
    pretty (JumpIfPrimitiveBool cond trueLabel falseLabel) =
        "if_primitive" <+> parens (pretty cond) <+> "goto" <+> pretty trueLabel <+> "else goto" <+> pretty falseLabel <> semi
    pretty (SetField fieldClass field fieldType val) =
        "this." <> pretty field <+> "=" <+> pretty val <> semi
    pretty (Super superName args) =
        "super" <> tupled (map pretty args) <> semi

instance Pretty Expr where
    pretty (LitInt i) = pretty i
    pretty (LitString t) = dquotes (pretty t)
    pretty (LitDouble d) = pretty d
    pretty (LitBool b) = if b then "true" else "false"
    pretty (PrimitiveLitBool b) = if b then "primitive_true" else "primitive_false"
    pretty (LitChar c) = squotes (pretty c)
    pretty LitUnit = "()"
    pretty (LocalVar u t) = pretty u <> ":" <> pretty t
    pretty (This _) = "this"
    pretty (FieldRef cls name _) = pretty cls <> "." <> pretty name
    pretty (New cls args) = "new" <+> pretty cls <> tupled (map (pretty . fst) args)
    pretty (GetField obj _ name _) = pretty obj <> dot <> pretty name
    pretty (BinaryOp op l r) = parens $ pretty l <+> pretty op <+> pretty r
    pretty (UnaryOp op e) = pretty op <> parens (pretty e)
    pretty (InstanceOf e t) = parens (pretty e) <+> "instanceof" <+> pretty t
    pretty (Call type' args) = prettyCall type' args
    pretty (MakeClosure targetClass targetMethod targetMethodDesc interface captures) =
        "makeClosure" <> tupled [pretty targetClass, pretty targetMethod, pretty targetMethodDesc, pretty interface, tupled (map (\(e, t) -> pretty e <+> ":" <+> pretty t) captures)]
    pretty (MakeConstructorClosure targetClass targetCtorDesc interface captures) =
        "makeCtorClosure" <> tupled [pretty targetClass, pretty targetCtorDesc, pretty interface, tupled (map (\(e, t) -> pretty e <+> ":" <+> pretty t) captures)]
    pretty (PrimOp op args) =
        pretty op <> tupled (map pretty args)
    pretty (Cast e t) = parens (pretty e <+> "as" <+> pretty t)

prettyCall :: CallType -> [Expr] -> Doc AnsiStyle
prettyCall type' args = pretty type' <> tupled (map pretty args)

instance Pretty CallType where
    pretty (InvokeStatic cls name _) = pretty cls <> dot <> pretty name
    pretty (InvokeVirtual target _ name _) = pretty target <> dot <> pretty name
    pretty (InvokeInterface target _ name _) = parens ("(interface)" <+> pretty target) <> dot <> pretty name

instance Pretty BinOp where
    pretty Add = "+"
    pretty Subtract = "-"
    pretty Multiply = "*"
    pretty Divide = "/"
    pretty And = "&&"
    pretty Or = "||"
    pretty Equals = "=="
    pretty NotEquals = "!="
    pretty LessThan = "<"
    pretty GreaterThan = ">"

instance Pretty Op where
    pretty Not = "!"
    pretty Negate = "-"
