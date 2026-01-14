module Elara.JVM.Lower.ADT (lowerDataCon) where

import Data.List (zipWith3)
import Effectful
import Elara.Core
import Elara.Data.Unique
import Elara.Data.Unique.Effect
import Elara.JVM.Emit.Types (stringTypeName)
import Elara.JVM.IR qualified as IR
import Elara.JVM.Lower.Monad
import Elara.JVM.Lower.Util
import JVM.Data.Abstract.Descriptor qualified as JVM
import JVM.Data.Abstract.Name
import JVM.Data.Abstract.Type qualified as JVM

type ADTInfo = (QualifiedClassName, [IR.Field])

-- | Build an instance method (non-static) from components
buildInstanceMethod :: Text -> [(Unique Text, JVM.FieldType)] -> JVM.ReturnDescriptor -> [IR.Block] -> IR.Method
buildInstanceMethod name args retDesc body =
    IR.Method
        { IR.methodName = name
        , IR.methodDesc = JVM.MethodDescriptor (map snd args) retDesc
        , IR.methodArgs = args
        , IR.methodBody = body
        , IR.methodIsStatic = False
        }

-- | Lower a single Data Constructor into a Class
lowerDataCon :: Lower r => QualifiedClassName -> DataCon -> Eff r IR.Class
lowerDataCon parentClass (DataCon conName conType _tyCon) = do
    let className = qualifiedTextToClass conName

    let fieldTypes = extractFieldTypes conType

    -- Generate fields (f0, f1, f2...)
    let fields = zipWith (\t i -> IR.Field (fieldNameForIndex i) t) fieldTypes [0 ..]

    constructor <- generateConstructor parentClass (className, fields)

    equalsMethod <- generateEqualsMethod (className, fields)
    elaraToStringMethod <- generateElaraToStringMethod (className, fields)
    toStringMethod <- generateToStringMethod (className, fields)
    pure $
        IR.Class
            { IR.className = className
            , IR.classSuper = parentClass
            , IR.classFields = fields
            , IR.classMethods = [equalsMethod, toStringMethod, elaraToStringMethod]
            , IR.classConstructors = [constructor]
            }

-- | Generate a constructor for a data constructor class.
generateConstructor ::
    Lower r =>
    -- | Parent class
    QualifiedClassName ->
    -- | Class name and fields
    ADTInfo ->
    -- | Generated constructor
    Eff r IR.Constructor
generateConstructor parentClass (className, fields) = do
    argsWithUniques <- forM fields $ \f -> do
        argUnique <- makeUnique (IR.fieldName f)
        pure (argUnique, IR.fieldType f, IR.fieldName f)

    let methodArgs = map (\(u, t, _) -> (u, t)) argsWithUniques
    let body =
            IR.Super parentClass []
                : [IR.SetField className fname t (IR.LocalVar u t) | (u, t, fname) <- argsWithUniques]
                ++ [IR.Return Nothing]

    entryLabel <- makeUnique "constructor_entry"
    pure $
        IR.Constructor
            { IR.constructorDesc = JVM.MethodDescriptor (map snd methodArgs) JVM.VoidReturn
            , IR.constructorArgs = methodArgs
            , IR.constructorBody = [IR.Block entryLabel body]
            }

-- | Generate a boolean equals(Object) method for data constructor classes.
generateEqualsMethod :: Lower r => ADTInfo -> Eff r IR.Method
generateEqualsMethod (className, fields) = do
    objParamUnique <- makeUnique "obj"
    let objFieldType = JVM.ObjectFieldType "java.lang.Object"

    otherUnique <- makeUnique "other"
    let thisClassType = JVM.ObjectFieldType className

    entryLabel <- makeUnique "equals_entry"
    checkFieldsLabel <- makeUnique "equals_check_fields"
    returnFalseLabel <- makeUnique "equals_return_false"
    returnTrueLabel <- makeUnique "equals_return_true"

    let instOfCheck = IR.InstanceOf (IR.LocalVar objParamUnique objFieldType) thisClassType

    let castExpr = IR.Cast (IR.LocalVar objParamUnique objFieldType) thisClassType

    -- Build field comparisons using java.util.Objects.equals for each field
    let objectsEqualsDesc =
            JVM.MethodDescriptor
                [JVM.ObjectFieldType "java.lang/Object", JVM.ObjectFieldType "java/lang/Object"]
                (JVM.TypeReturn (JVM.PrimitiveFieldType JVM.Boolean))

    let compareField (IR.Field fieldName fieldType) =
            IR.Call
                (IR.InvokeStatic "java/util/Objects" "equals" objectsEqualsDesc)
                [ IR.GetField (IR.This thisClassType) (JVM.ClassInfoType className) fieldName fieldType
                , IR.GetField (IR.LocalVar otherUnique thisClassType) (JVM.ClassInfoType className) fieldName fieldType
                ]

    -- Generate labels for each field check
    fieldCheckLabels <- forM (zip [0 ..] fields) $ \(i, _) ->
        makeUnique ("equals_check_f" <> show (i :: Int))

    -- Build method body with short-circuit evaluation:
    -- Entry: if (!(obj instanceof ClassName)) goto returnFalse; else goto checkFields
    -- CheckFields: other = (ClassName)obj; goto first field check (or returnTrue if no fields)
    -- For each field: if (!Objects.equals(this.fN, other.fN)) goto returnFalse; else goto next
    -- ReturnTrue: return true
    -- ReturnFalse: return false

    let firstCheckOrTrue = case fieldCheckLabels of
            [] -> returnTrueLabel
            (first : _) -> first

    -- Note: instOfCheck (instanceof) returns primitive boolean, not Elara Bool
    -- So we use JumpIfPrimitiveBool, not JumpIf
    let entryBlock =
            IR.Block
                entryLabel
                [ IR.JumpIfPrimitiveBool instOfCheck checkFieldsLabel returnFalseLabel
                ]

    let castBlock =
            IR.Block
                checkFieldsLabel
                [ IR.Assign otherUnique thisClassType castExpr
                , IR.Jump firstCheckOrTrue
                ]

    -- Generate field check blocks with short-circuit
    -- Note: compareField returns primitive boolean from Objects.equals,
    -- so we use JumpIfPrimitiveBool instead of JumpIf
    let fieldChecks = zipWith3 makeFieldCheck fieldCheckLabels fields (drop 1 fieldCheckLabels ++ [returnTrueLabel])
        makeFieldCheck label field nextLabel =
            IR.Block
                label
                [ IR.JumpIfPrimitiveBool (compareField field) nextLabel returnFalseLabel
                ]

    let returnTrueBlock =
            IR.Block
                returnTrueLabel
                [ IR.IReturn (IR.PrimitiveLitBool True)
                ]

    let returnFalseBlock =
            IR.Block
                returnFalseLabel
                [ IR.IReturn (IR.PrimitiveLitBool False)
                ]

    let methodBody = [entryBlock, castBlock] ++ fieldChecks ++ [returnTrueBlock, returnFalseBlock]
    let primitiveBooleanReturn = JVM.TypeReturn (JVM.PrimitiveFieldType JVM.Boolean)

    pure $ buildInstanceMethod "equals" [(objParamUnique, objFieldType)] primitiveBooleanReturn methodBody

-- | Generates a method Elara.String toElaraString() for the given ADTInfo.
generateElaraToStringMethod :: Lower r => ADTInfo -> Eff r IR.Method
generateElaraToStringMethod (className, fields) = do
    let elaraStringType = JVM.ObjectFieldType stringTypeName
    let elaraStringReturn = JVM.TypeReturn elaraStringType
    let thisClassType = JVM.ObjectFieldType className
    let simpleClassName = case className of
            QualifiedClassName _ (ClassName name) -> name
    let concatDesc =
            JVM.MethodDescriptor
                [JVM.ObjectFieldType stringTypeName]
                (JVM.TypeReturn (JVM.ObjectFieldType stringTypeName))

    entryLabel <- makeUnique "toElaraString_entry"

    methodBody <-
        if null fields
            then
                pure
                    [ IR.Block
                        entryLabel
                        [ IR.Return (Just (IR.LitString simpleClassName))
                        ]
                    ]
            else do
                resultUnique <- makeUnique "result"
                let elaraStringType = JVM.ObjectFieldType stringTypeName
                let openParen = simpleClassName <> "("

                -- Build field string representations
                fieldStringExprs <- forM (zip [0 ..] fields) $ \(i :: Int, IR.Field fieldName fieldType) -> do
                    let fieldAccess = IR.GetField (IR.This thisClassType) (JVM.ClassInfoType className) fieldName fieldType

                    -- Convert field to Elara/String using PrimOp ToString
                    let fieldStr = IR.PrimOp IR.ToString [fieldAccess]

                    -- Add comma prefix if not the first field
                    if i == 0
                        then pure fieldStr
                        else do
                            let commaStr = IR.LitString ", "

                            -- Concatenate ", " + fieldStr (both as Elara/String)
                            pure $
                                IR.Call
                                    (IR.InvokeVirtual commaStr stringTypeName "concat" concatDesc)
                                    [fieldStr]

                -- Concatenate all parts using Elara/String.concat
                let buildConcat :: IR.Expr -> [IR.Expr] -> IR.Expr
                    buildConcat acc [] =
                        IR.Call (IR.InvokeVirtual acc stringTypeName "concat" concatDesc) [IR.LitString ")"]
                    buildConcat acc (x : xs) =
                        let nextAcc = IR.Call (IR.InvokeVirtual acc stringTypeName "concat" concatDesc) [x]
                         in buildConcat nextAcc xs

                let finalResult = buildConcat (IR.LitString openParen) fieldStringExprs

                pure
                    [ IR.Block
                        entryLabel
                        [ IR.Assign resultUnique elaraStringType finalResult
                        , IR.Return (Just (IR.LocalVar resultUnique elaraStringType))
                        ]
                    ]

    pure $ buildInstanceMethod "toElaraString" [] elaraStringReturn methodBody

generateToStringMethod :: Lower r => ADTInfo -> Eff r IR.Method
generateToStringMethod (className, _fields) = do
    -- just call toElaraString().toString()
    let javaStringReturn = JVM.TypeReturn (JVM.ObjectFieldType "java/lang/String")
    let javaStringDesc = JVM.MethodDescriptor [] javaStringReturn
    let elaraStringType = JVM.ObjectFieldType stringTypeName
    let thisClassType = JVM.ObjectFieldType className
    entryLabel <- makeUnique "toString_entry"
    let callToElaraString =
            IR.Call
                (IR.InvokeVirtual (IR.This thisClassType) className "toElaraString" (JVM.MethodDescriptor [] (JVM.TypeReturn elaraStringType)))
                []
    let callToString =
            IR.Call
                (IR.InvokeVirtual callToElaraString stringTypeName "toString" javaStringDesc)
                []
    let methodBody = [IR.Block entryLabel [IR.Return (Just callToString)]]
    pure $ buildInstanceMethod "toString" [] javaStringReturn methodBody
