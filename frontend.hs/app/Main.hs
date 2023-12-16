module Main where

import Data.Text (pack)
import Lexer (lexTokens)
import qualified Text.Megaparsec as M
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  let toks = lexTokens "test" (pack "lol[123]")
  case toks of
    Left err -> putStrLn $ errorBundlePretty err
    Right toks -> print toks
