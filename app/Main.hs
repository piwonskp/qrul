module Main where

import Control.Monad.Except
import Control.Exception (Exception, displayException)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.Char8 as B
import Data.Void (Void)
import LLVM.Module (withModuleFromAST, moduleLLVMAssembly,
                    writeLLVMAssemblyToFile, File(..))
import LLVM.Context (withContext)
import LLVM.AST (Module)
import Text.Megaparsec.Error (ParseErrorBundle)
import System.Environment (getArgs)
import System.FilePath.Posix ((-<.>))

import Codegen (compileAST)
import Parser (parse)
import Types
import AST (CStmt)


fromAST f ast = withContext $ \context ->
  withModuleFromAST context ast f

displayError :: Exception e => e -> a
displayError = error . displayException
errorOr :: Exception e => (a -> b) -> Either e a -> b
errorOr = either displayError

compileQ :: FilePath -> [CStmt] -> Either CodegenError Module
compileQ fname = runQrul . compileAST fname
parseQ :: FilePath -> String -> Either (ParseErrorBundle String Void) [CStmt]
parseQ fname = runQrul . parse fname
compile :: FilePath -> String -> Module
compile fname = errorOr (errorOr id . compileQ fname) . parseQ fname

codegen :: Module -> IO ByteString
codegen = fromAST moduleLLVMAssembly
printCodegen :: Module -> IO ()
printCodegen mod = codegen mod >>= putStrLn . toString

toLLFile = File . (-<.> ".ll")
writeToFile :: FilePath -> Module -> IO ()
writeToFile fname = fromAST $ writeLLVMAssemblyToFile (toLLFile fname)

printAST fname = errorOr print . parseQ fname

compileAnd foo fname = foo . compile fname
compileAnd' foo fname = foo fname . compile fname

opts ["parse"] = printAST
opts ["print"] = compileAnd printCodegen
opts [] = compileAnd' writeToFile

main :: IO ()
main = do
  (fname:args) <- getArgs
  opts args fname =<< readFile fname
