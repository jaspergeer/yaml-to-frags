module Main where

import Data.Yaml (decodeAllFileEither)
import Prettyprinter (Pretty (pretty))
import System.Environment (getArgs)
import YamlFragment (YamlFragment, prettyPieces)
import Prettyprinter.Render.Text (putDoc)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "yaml-to-frags: Not enough arguments"
        x : _ -> do
            decoded <- decodeAllFileEither x
            case decoded of
                Left e -> putStr $ show e
                Right fragments -> putDoc $ prettyPieces $ map pretty (fragments :: [YamlFragment])
