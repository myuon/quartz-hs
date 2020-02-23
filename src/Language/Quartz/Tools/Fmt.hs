module Language.Quartz.Tools.Fmt where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import Data.Maybe (isJust, fromJust)
import Data.Vector.PushBack
import Data.Text.Prettyprint.Doc
import Language.Quartz.Tools.FmtLexer

data Code
  = AToken Lexeme
  | Sequence (V.Vector Code)
  -- open punctuation close
  | Block String String String (V.Vector Code)
  | Scope String String String (V.Vector Code)
  deriving (Eq, Show)

data StackElement s
  = STokens (PBVector s Code)
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
      Lexeme _ (TSymbol t) | t == "{" -> start stack token
      Lexeme _ (TSymbol t) | t == ";" -> continue stack
      Lexeme _ (TSymbol t) | t == "}" -> close stack "{" ";" "}"
      _ -> do
        popped <- pop stack
        case popped of
          Just (STokens v) -> do
            push v     (AToken token)
            push stack (STokens v)
          Just (SCode c) -> do
            push stack (SCode c)

            v <- new 0
            push v     (AToken token)
            push stack (STokens v)

  vs <- fmap V.toList $ toVector stack
  fmap (V.toList . V.concat . map fromJust . filter isJust)
    $ forM vs
    $ \v -> case v of
        STokens p -> do
          v <- toVector p
          if V.null v then return Nothing else return (Just v)
        SCode c -> return (Just $ V.singleton c)
 where
  start stack token = do
    v <- new 0
    push v     (AToken token)
    push stack (STokens v)
    w <- new 0
    push stack (STokens w)

  continue stack = do
    v <- new 0
    push stack (STokens v)

  close stack open punct close = do
    vs     <- popUntil stack (TSymbol open)
    popped <- pop stack
    case popped of
      Just (STokens w) -> do
        push w     (Scope open punct close $ V.reverse $ V.fromList vs)
        push stack (STokens w)

  popUntil stack ch = do
    popped <- pop stack
    case popped of
      Just (STokens vs') -> do
        vs <- toVector vs'
        if isTokenOf ch vs
          then return []
          else fmap (Sequence vs :) $ popUntil stack ch
      Just (SCode c) -> do
        fmap (c :) $ popUntil stack ch

isTokenOf ch vs = if V.length vs == 1
  then case (vs V.! 0) of
    AToken t | tokenOfLexeme t == ch -> True
    _ -> False
  else False

--formatDoc :: String -> Either String (Doc a)
--formatDoc = pretty . alexScanTokens
