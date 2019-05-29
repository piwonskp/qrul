{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Codegen where

import Control.Monad.Except (MonadError, throwError)
import LLVM.IRBuilder.Module (MonadModuleBuilder, ModuleBuilderT,
                              buildModuleT, ParameterName(..),
                              emitDefn)
import LLVM.IRBuilder.Instruction (alloca, store, ret, load)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Monad (IRBuilderT, runIRBuilderT, emptyIRBuilder,
                             fresh, named, currentBlock, PartialBlock(..),
                             builderBlock, liftIRState)
import LLVM.AST (Type(..), Module(..), FloatingPointType(..),
                Named(Do), Name(..), BasicBlock(..),
                Operand(LocalReference, ConstantOperand),
                Definition(..), functionDefaults, Parameter(..),
                Terminator(Ret))
import LLVM.AST.Float (SomeFloat(..))
import LLVM.AST.Global (Global(..))
import qualified LLVM.AST.Constant as C
import LLVM.AST.Typed (typeOf)
import LLVM.AST.Type (ptr)
import Control.Monad.State.Class (gets)
import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Control.Monad.HT (liftJoin2)
import Text.Megaparsec.Pos (SourcePos, initialPos)

import AST
import Types


emptyCodegen = CodegenState []
type CodegenMonad = Qrul CodegenError

newtype CGModule a = CGModule { unCGModule :: ModuleBuilderT (StateT CodegenState CodegenMonad) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadError CodegenError, MonadModuleBuilder, MonadState CodegenState)

type CGBlock = IRBuilderT CGModule

runCGModule :: FilePath -> CGModule a -> CodegenMonad Module
runCGModule fname m = fst <$> runStateT (buildModuleT "Dddd" $ unCGModule m) (emptyCodegen $ initialPos fname)

compileAST :: FilePath -> [CStmt] -> CodegenMonad Module
compileAST fname = runCGModule fname . codegenTop

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
  symtab       :: SymbolTable,
  sourcePos    :: SourcePos
  } deriving Show

retType :: Maybe PartialBlock -> LLVM.AST.Type
retType (Just (PartialBlock _ _ (Just (Do (LLVM.AST.Ret (Just expr) _))))) = typeOf expr
retType _ = VoidType

-- LLVM.IRBuilder.Module.function replacement
function label argtys body = do
  let tys = fst <$> argtys
  ((paramNames, retty), blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
--     Get function type from terminator
    retty <- retType <$> liftIRState (gets builderBlock)
    return (paramNames, retty)
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label

codegenTop :: [CStmt] -> CGModule Operand
codegenTop stmts = main instructions
    where
        main = function (Name "main") []
        instructions _ = mapM_ (setSourcePos >=> cgen) stmts

assign :: String -> Operand -> CGBlock ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }

store' a = store a 0
load' = flip load 0

setSourcePos (Context pos stmt) = modify (\s -> s { sourcePos = pos }) >> return stmt

cgen :: Stmt -> CGBlock ()
cgen (Declaration type_ name) = alloca (typeToLLVM type_) Nothing 0 >>= assign name
cgen (Definition name val) = liftJoin2 store' (getVar name) (valToLLVM name val)
cgen (AST.Ret e) = ret =<< mkExpr e

getRefType name = llvmToType . unwrap <$> getVar name
  where unwrap (LocalReference (PointerType t _) _) = t
llvmToType IntType = Int
llvmToType DoubleType = Float

getVar :: String -> CGBlock Operand
getVar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> (throwError . CodegenError (err var)) =<< gets sourcePos
  where err var = "Local variable not in scope: " ++ show var

checkType declared exprType | declared == exprType = return ()
  | otherwise = err
  where
    err :: CGBlock ()
    err = (throwError . CodegenError msg) =<< gets sourcePos
    msg = "Type mismatch. Declared: " ++ show declared ++ ". Got: " ++ show exprType

valToLLVM :: String -> Expr -> CGBlock Operand
valToLLVM name expr = do
  join $ checkType <$> getRefType name <*> getExprType expr
  mkExpr expr

pattern DoubleType = FloatingPointType DoubleFP
pattern IntType = IntegerType 32

getExprType (Var name) = getRefType name
getExprType (Constant c) = return $ valueToType c
valueToType (IntConst _) = Int
valueToType (FloatConst _) = Float

mkExpr (Var name) = getVar name >>= load'
mkExpr (Constant c) = constant c
constant (IntConst n) = int32 n
constant (FloatConst n) = double n

typeToLLVM Float = DoubleType
typeToLLVM Int = IntType
