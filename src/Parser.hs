module Parser where

import Control.Monad (void, liftM2)
import Control.Monad.Except (liftEither)
import Control.Arrow
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (Parsec(..), takeWhile1P, takeWhileP,
                        (<|>), empty, try, many,
                        some, eof, getSourcePos, option,
                        ParseErrorBundle, runParser)
import Text.Megaparsec.Char (letterChar, string, eol, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List.Extra (snoc)
import Text.Megaparsec.Stream

import AST
import Types (Qrul)


type Parser a = Parsec Void String a

isEol = (== '\n')
space1 = void $ takeWhile1P (Just "white space") (isSpace &&& (not . isEol) >>> uncurry (&&))
sc = L.space space1 empty empty
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

reservedNames = ["Int", "Float"]
integer = lexeme L.decimal
float = lexeme L.float

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> takeWhileP (Just "identifier") (not . isSpace)
    check :: String -> Parser String
    check x = if x `elem` reservedNames
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> space1)

value :: Parser Expr
value = try (Constant . FloatConst <$> float)
    <|> Constant . IntConst <$> integer
type_ = (rword "Int" >> return Int) <|>
    (rword "Float" >> return Float)

definition :: Parser Stmt
definition = Definition <$> identifier <*> expr

declaration :: Parser Stmt
declaration = Declaration <$> type_ <*> identifier

addContextTo parser = Context <$> getSourcePos <*> parser

var :: Parser Expr
var = Var <$> identifier

expr :: Parser Expr
expr = try value <|> var

statement :: Parser CStmt
statement = addContextTo $ declaration
    <|> definition

ret :: Parser Stmt
ret = Ret <$> expr


parser :: Parser [CStmt]
parser = liftM2 (++) statements optionalRet <* delimiter
    where
    delimiter = try space <|> eof
    statements = many $ try $ space >> statement <* eol
    optionalRet = option [] $ fmap (:[]) $ space >> addContextTo ret

parse :: FilePath -> String -> Qrul (ParseErrorBundle String Void) [CStmt]
parse fn = liftEither . runParser parser fn
