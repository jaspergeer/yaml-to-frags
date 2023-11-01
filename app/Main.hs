module Main where

import System.Environment (getArgs)
import Data.Yaml (decodeFileEither, decodeAllFileEither)
import Data.Text (unpack)
import Prettyprinter (Pretty(pretty))
import YamlFragment (YamlFragment)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "yamlToFrags: Not enough arguments"
        x : _ -> do
          decoded <- decodeAllFileEither x
          case decoded of
            Left e -> putStr $ show e
            Right (fragment : _) -> putStr $ show $ pretty (fragment :: YamlFragment)
