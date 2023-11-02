{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (toUpper)
import qualified Data.Text as Text
import Data.Yaml (decodeAllFileThrow)
import Prettyprinter (Pretty (pretty))
import System.Environment (getArgs)
import TestGen (prettyModule, prettyPieces, prettyTest)
import YamlFragment (YamlFragment)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "yaml-to-frags: Not enough arguments"
        moduleName : testNames -> do
            decoded <- traverse decodeAllFileThrow fileNames
            let tests = map (\(n, frags) -> prettyTest n $ prettyPieces $ map pretty (frags :: [YamlFragment])) $ zip convertedTestNames decoded
            putStr $ show $ prettyModule (Text.pack moduleName) tests
          where
            fileNames = map (++ ".yaml") testNames

            upperFirst s = let firstChar = Text.head s in Text.cons (toUpper firstChar) (Text.tail s)

            camelCase (x : xs) = mconcat $ x : map upperFirst xs
            camelCase _ = mempty

            convertedTestNames = map (camelCase . Text.splitOn "-" . Text.pack) testNames
