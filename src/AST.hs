module AST where

import Text.Megaparsec (SourcePos(..))


type Name = String
data Type = Int | Float
    deriving (Show, Eq)
data Value = IntConst Integer | FloatConst Double
    deriving (Show)

data Expr = Constant Value | Var Name
    deriving (Show)

data Stmt = Seq [Stmt]
    | Declaration Type Name
    | Definition Name Expr
    | Ret Expr
    deriving (Show)

data Context a = Context SourcePos a
    deriving (Show)

type CStmt = Context Stmt
