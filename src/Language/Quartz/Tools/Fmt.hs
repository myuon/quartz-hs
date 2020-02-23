module Language.Quartz.Tools.Fmt where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import Data.Maybe (isJust, fromJust)
import Data.Vector.PushBack
import Data.Text.Prettyprint.Doc
import Language.Quartz.Tools.FmtLexer

data Code
  = Tokens (V.Vector Lexeme)
  -- open punctuation close
  | Block String String String (V.Vector Code)
  | Scope String String String (V.Vector Code)
  deriving (Eq, Show)

data StackElement s
  = STokens (PBVector s Lexeme)
  | SCode Code

parse :: [Lexeme] -> [Code]
parse tokens = runST $ do
  stack <- new 0
  do
    v <- new 0
    push stack $ STokens v

  forM_ tokens $ \token -> do
    case token of
      Lexeme _ (TSymbol t) | t == "," -> continue stack
      Lexeme _ (TSymbol t) | t == "(" -> start stack token
      Lexeme _ (TSymbol t) | t == ")" -> close stack "(" "," ")"
      Lexeme _ (TSymbol t) | t == "<" -> start stack token
      Lexeme _ (TSymbol t) | t == ">" -> close stack "<" "," ">"
      _ -> do
        popped <- pop stack
        case popped of
          Just (STokens v) -> do
            push v     token
            push stack (STokens v)
          Just (SCode c) -> do
            push stack (SCode c)

            v <- new 0
            push v     token
            push stack (STokens v)

  vs <- fmap V.toList $ toVector stack
  fmap (map fromJust . filter isJust) $ forM vs $ \v -> case v of
    STokens p -> do
      v <- toVector p
      if V.null v then return Nothing else return (Just $ Tokens v)
    SCode c -> return (Just c)
 where
  start stack token = do
    v <- new 0
    push v     token
    push stack (STokens v)
    w <- new 0
    push stack (STokens w)

  continue stack = do
    v <- new 0
    push stack (STokens v)

  close stack open punct close = do
    vs <- popUntil stack (TSymbol open)
    push stack (SCode $ Scope open punct close $ V.reverse $ V.fromList vs)

  popUntil stack ch = do
    popped <- pop stack
    case popped of
      Just (STokens vs') -> do
        vs <- toVector vs'
        if V.length vs == 1 && tokenOfLexeme (vs V.! 0) == ch
          then return []
          else fmap (Tokens vs :) $ popUntil stack ch
      Just (SCode c) -> do
        fmap (c :) $ popUntil stack ch

--formatDoc :: String -> Either String (Doc a)
--formatDoc = pretty . alexScanTokens
