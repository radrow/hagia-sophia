{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Sophia.Syntax where

newtype AST = AST [TopDecl]

data TopDecl
  = Namespace {decls :: [Decl]}

data Decl
  = DeclFun FunDecl

type Block = [Stmt]

data Stmt
  = StmtExpr Expr
  -- | StmtLetDef LetDef
  -- | StmtIf Expr Block
  -- | StmtIfElse [(Expr, Block)] Block

data Expr
  = ExprVar{expVarId :: VarId}
  | ExprLit{exprLit :: Lit}
  | ExprOp{exprOp :: Op, exprOpL :: Expr, exprOpR :: Expr}
  | ExprApp{exprAppFun :: Expr, exprAppArgs :: [Expr]}
  | ExprBlock{exprBlock :: Block, exprBlockRet :: Expr}

data Lit
  = LitInt{litInt :: Integer}
  | LitBool{litBool :: Bool}

data Op = Plus

newtype VarId  = VarId  {vidText  :: String}
newtype TVarId = TVarId {tvidText :: String}

data LetDef
  = LetDefFun{ldFunDecl :: FunDecl}
  | LetDefVal{ldValDecl :: ValDecl}

data FunDecl = FunDecl
  { fdName :: VarId
  , fdArgs :: [Arg]
  , fdRetType :: Type
  , fdBody :: Block
  }

data ValDecl = ValDecl
  { vdName :: VarId
  , vdType :: Type
  , vdValue :: Expr
  }

data Arg = Arg
  { argName :: VarId
  , argType :: Type
  }

data Type
  = TUVar{tuvarId :: TVarId}
  | TVar{tvarId :: TVarId}
  | TFun{tfunArgs :: [Type], tfunRet :: Type}

pattern (:->) :: [Type] -> Type -> Type
pattern ts :-> ret = TFun ts ret
