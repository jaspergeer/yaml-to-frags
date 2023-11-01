module Main where

import Data.Yaml (decodeAllFileEither)
import Prettyprinter (Pretty (pretty))
import System.Environment (getArgs)
import YamlFragment (YamlFragment, prettyPieces)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "yamlToFrags: Not enough arguments"
        x : _ -> do
            decoded <- decodeAllFileEither x
            case decoded of
                Left e -> putStr $ show e
                Right fragments -> putStr $ show $ prettyPieces $ map pretty (fragments :: [YamlFragment])
