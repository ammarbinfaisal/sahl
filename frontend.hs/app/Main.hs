module Main where

import Control.Monad.State (MonadTrans (..), StateT (runStateT), runState)
import Data.Text (pack)
import Lexer (lexTokens)
import Parser (expr)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Megaparsec (errorBundlePretty)
import qualified Text.Megaparsec as M
import Text.Pretty.Simple (pPrint)

parse :: String -> IO ()
parse input = do
  let toks = lexTokens "test" (pack input)
  case toks of
    Left err -> putStrLn $ errorBundlePretty err
    Right toks -> do
      pPrint toks
      let p = M.runParser expr "<>" toks
      case p of
        Left err -> print "error"
        Right res -> pPrint res

main :: IO ()
main = do
  runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "λ "
      case minput of
        Nothing -> return ()
        Just input -> do
          lift $ parse input
          loop
