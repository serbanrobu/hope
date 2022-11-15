module Expr (fromAst) where

import Ast (Ast)
import qualified Ast
import Data.List (findIndex)
import Relude

data Expr
  = App Expr Expr
  | Lam (Maybe Text) Expr Expr
  | Pi (Maybe Text) Expr Expr
  | Unknown
  | Var Int
  deriving (Show, Eq)

type Context = [(Text, Expr, Maybe Expr)]

fromAst :: Ast -> StateT Context (Either Text) (Expr, Expr)
fromAst (Ast.App f x) = do
  (f', f'T) <- fromAst f
  case f'T of
    (Pi _ t b) -> do
      (x', x'T) <- fromAst x
      unless (fitInto t x'T) . lift . Left $ show x'T <> " doesn't fit into " <> show t
      pure (App f' x', shift 0 (-1) $ substitute 0 (shift 0 1 x') b)
    _ ->
      lift . Left $
        "Expecting a function, found: " <> show f' <> " : " <> show f'T
fromAst (Ast.Lam m t b) = do
  (t', _) <- fromAst t
  whenJust m $ \n -> modify ((n, t', Nothing) :)
  (b', b'T) <- fromAst b
  pure (Lam m t' b', Pi m t' b'T)
fromAst (Ast.Pi m t b) = do
  (t', _) <- fromAst t
  whenJust m $ \n -> modify ((n, t', Nothing) :)
  (b', _) <- fromAst b
  pure (Pi m t' b', Unknown)
fromAst Ast.Unknown = pure (Unknown, Unknown)
fromAst (Ast.Var n) = do
  ctx <- get
  i <-
    lift
      . maybeToRight "Not found in context"
      $ findIndex (\(n', _, _) -> n' == n) ctx
  (_, t, v) <-
    lift
      . maybeToRight "Index out of bounds"
      $ ctx !!? i
  pure (maybe (Var i) (shift 0 (i + 1)) v, shift 0 (i + 1) t)

shift :: Int -> Int -> Expr -> Expr
shift c d (App f x) = App (shift c d f) $ shift c d x
shift c d (Lam Nothing t b) = Lam Nothing (shift c d t) $ shift c d b
shift c d (Lam m t b) = Lam m (shift c d t) $ shift (c + 1) d b
shift c d (Pi Nothing t b) = Pi Nothing (shift c d t) $ shift c d b
shift c d (Pi m t b) = Pi m (shift c d t) $ shift (c + 1) d b
shift c d t@(Var i) = if i < c then t else Var $ i + d
shift _ _ t = t

substitute :: Int -> Expr -> Expr -> Expr
substitute i a (App f x) = App (substitute i a f) $ substitute i a x
substitute i a (Lam Nothing t b) = Lam Nothing (substitute i a t) $ substitute i a b
substitute i a (Lam m t b) = Lam m (substitute i a t) $ substitute (i + 1) (shift 0 1 a) b
substitute i a (Pi Nothing t b) = Pi Nothing (substitute i a t) $ substitute i a b
substitute i a (Pi m t b) = Pi m (substitute i a t) $ substitute (i + 1) (shift 0 1 a) b
substitute i a t@(Var i') = if i' == i then a else t
substitute _ _ t = t

fitInto :: Expr -> Expr -> Bool
fitInto (App f1 x1) (App f2 x2) = fitInto f1 f2 && fitInto x1 x2
fitInto (Lam _ t1 b1) (Lam _ t2 b2) = fitInto t1 t2 && fitInto b1 b2
fitInto (Pi _ t1 b1) (Pi _ t2 b2) = fitInto t1 t2 && fitInto b1 b2
fitInto Unknown _ = True
fitInto e1 e2 = e1 == e2
