{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Latte.Frontend.Parse where

import           Data.Functor(void, ($>))
import           Control.Applicative        (liftA2)
import           Control.Monad
import           Control.Monad.Identity
import           Data.Void(Void)
import           Data.List as DL
import           Data.List.NonEmpty as NE
import           Data.Text
import           Data.Bifunctor
import           Text.Megaparsec hiding (sepBy1)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Prelude hiding (lex, LT, GT, EQ)

import Language.Sophia.Syntax

type Parser = ParsecT Void Text Identity
runLatteParser :: Parser a -> FilePath -> Text -> Either String a
runLatteParser p filename inp = first
  errorBundlePretty
  (parse (skip *> p <* eof) filename inp)


keywords :: [String]
keywords =
  [ "namespace", "contract"
  , "let", "function"
  , "if" , "else", "switch"
  , "true", "false"
  ]


validId :: Parser String -> Parser String
validId ps = try $ do
  s <- ps
  when (s `elem` keywords) $
    fail ("invalid id: " ++ s)
  return s
{-# INLINE validId #-}


skip :: Parser ()
skip = L.space
  (void spaceChar)
  (L.skipLineComment "//" <|> L.skipLineComment "#")
  (L.skipBlockComment "/*" "*/")


lex :: Parser a -> Parser a
lex = L.lexeme skip
{-# INLINE lex #-}


lId :: Parser String
lId = lex $ liftA2 (:) lowerChar (many alphaNumChar)
{-# INLINE lId #-}


uId :: Parser String
uId = lex $ liftA2 (:) upperChar (many alphaNumChar)
{-# INLINE uId #-}


signed :: Parser Integer
signed = lex $ L.decimal
{-# INLINE signed #-}


operator :: Text -> Parser ()
operator o =
  lex $ try $ string o *> notFollowedBy (oneOf ("=+-/*%:\\&.|^<>" :: String))
{-# INLINE operator #-}


symbol :: Text -> Parser Text
symbol = L.symbol skip
{-# INLINE symbol #-}


word :: Text -> Parser ()
word w = lex $ try $ string w >> notFollowedBy alphaNumChar >> skip
{-# INLINE word #-}


paren :: Parser a -> Parser a
paren = between (L.symbol skip "(") (L.symbol skip ")")
{-# INLINE paren #-}


brac :: Parser a -> Parser a
brac = between (L.symbol skip "{") (L.symbol skip "}")
{-# INLINE brac #-}


infixL :: Parser (a -> b -> a) -> Parser b -> a -> Parser a
infixL op p x = do
  f <- op
  y <- p
  let r = f x y
  infixL op p r <|> return r


varId :: Parser VarId
varId = VarId <$> validId lId

tVarId :: Parser TVarId
tVarId = TVarId <$> validId lId


lit :: Parser Lit
lit = choice
  [ LitInt <$> signed
  , LitBool <$> ((True <$ word "true") <|> (False <$ word "false"))
  ]

op :: Parser Op
op = choice
  [ Plus  <$ operator "+"
  ]

type_ :: Parser Type
type_ = choice
  [ char '\'' *> (TUVar <$> tVarId)
  , TVar <$> tVarId
  , do
      args <- paren (sepBy type_ (symbol ",")) <* operator "=>"
      ret <- type_
      pure TFun{tfunArgs = args, tfunRet = ret}
  ]

arg :: Parser Arg
arg = do
  n <- varId
  operator ":"
  t <- type_
  pure Arg{argName = n, argType = t}

valDecl :: Parser ValDecl
valDecl = do
  n <- varId
  operator ":"
  t <- type_
  operator "="
  e <- expr
  pure ValDecl{vdName = n, vdType = t, vdValue = e}

funDecl :: Parser FunDecl
funDecl = do
  n <- varId
  a <- paren (sepBy arg (symbol ","))
  operator ":"
  t <- type_
  operator "="
  b <- block
  pure FunDecl{fdName = n, fdArgs = a, fdRetType = t, fdBody = b}

letDef :: Parser LetDef
letDef = do
  word "let"
  choice
    [ LetDefFun <$> funDecl
    , LetDefVal <$> valDecl
    ]

expr :: Parser Expr
expr = choice
  [ ExprVar <$> varId
  , ExprLit <$> lit
  , paren $ choice
    [ do
        o <- op
        a1 <- expr
        a2 <- expr
        as <- many expr
        pure $ Prelude.foldl
          (\acc e ->
              ExprOp{exprOp = o, exprOpL = acc, exprOpR = e})
          ExprOp{exprOp = o, exprOpL = a1, exprOpR = a2}
          as
    , do
        f <- expr
        as <- many expr
        pure ExprApp{exprAppFun = f, exprAppArgs = as}
    ]
  ]

block :: Parser Block
block = brac (sepBy stmt (symbol ";"))

stmt :: Parser Stmt
stmt = choice
  [ StmtExpr <$> expr
  ]
