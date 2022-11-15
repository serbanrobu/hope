module Ast (Ast (..)) where

import Relude
import Text.Show (ShowS, showChar, showParen, showString, shows, showsPrec)

data Ast
  = App Ast Ast
  | Lam (Maybe Text) Ast Ast
  | Pi (Maybe Text) Ast Ast
  | Unknown
  | Var Text

varToText :: Maybe Text -> Text
varToText = fromMaybe "_"

showText :: Text -> ShowS
showText = showString . toString

showAbsHead :: Text -> Ast -> ShowS
showAbsHead v Unknown = showText v
showAbsHead v t =
  showChar '('
    . showText v
    . showString " : "
    . shows t
    . showChar ')'

showAbsBody :: Ast -> ShowS
showAbsBody b = showString ". " . shows b

showLamBody :: Ast -> ShowS
showLamBody (Lam v t b) = showChar ' ' . showAbsHead (varToText v) t . showLamBody b
showLamBody b = showAbsBody b

showArr :: Int -> Ast -> Ast -> ShowS
showArr p a b =
  showParen (p > arrPrec) $
    showsPrec (arrPrec + 1) a
      . showString " → "
      . showsPrec arrPrec b
  where
    arrPrec = 9

showPiBody :: Ast -> ShowS
showPiBody (Pi Nothing t b) = showString ". " . showArr 0 t b
showPiBody (Pi (Just v) t b) = showChar ' ' . showAbsHead v t . showPiBody b
showPiBody b = showAbsBody b

instance Show Ast where
  showsPrec p (App f a) =
    showParen (p > appPrec) $
      showsPrec appPrec f
        . showChar ' '
        . showsPrec (appPrec + 1) a
    where
      appPrec = 10
  showsPrec p (Lam v t b) =
    showParen (p > 0) $
      showString "λ " . showAbsHead (varToText v) t . showLamBody b
  showsPrec p (Pi Nothing a b) = showArr p a b
  showsPrec p (Pi (Just v) t b) =
    showParen (p > 0) $
      showString "Π " . showAbsHead v t . showPiBody b
  showsPrec _ Unknown = showChar '?'
  showsPrec _ (Var n) = showString $ toString n
