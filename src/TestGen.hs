{-# LANGUAGE OverloadedStrings #-}

module TestGen where

import Data.Text (Text)
import Prettyprinter (Doc, Pretty (..), hcat, hsep, indent, line, nest, vsep, (<+>), dquotes)

prettyClass :: [Text] -> Doc ann -> Doc ann
prettyClass qualifiedIdentifier body =
    vsep $
        [ "class" <+> hsep (map pretty qualifiedIdentifier) <+> "{"
        , indent 2 body
        , "}"
        ]

prettyPieces :: [Doc ann] -> Doc ann
prettyPieces frags =
    hsep ["val", "pieces", "=", "List("]
        <> nest 2 (hcat (zipWith (<>) ("\n" : repeat ",\n") frags))
        <> line
        <> ")"

prettyTest :: Text -> Doc ann -> Doc ann
prettyTest name pieces =
    vsep $
        [ dquotes (pretty name) <+> "in" <+> "{"
        , mempty
        , nest 2 pieces
        , mempty
        , indent 2 $ hsep ["val", "out", "=", "Mason(pieces.head, pieces.tail, \"test_output\")"]
        , "}"
        ]

prettyModule :: Text -> [Doc ann] -> Doc ann
prettyModule name tests =
    vsep $
        [ "package" <+> "edu.tufts.cs.javasynth"
        , "package" <+> "synthesis"
        , mempty
        , "import" <+> "source.frag"
        , "import" <+> "org.scalatest.freespec.AnyFreeSpec"
        , "import" <+> "org.scalatest.matchers.should"
        , mempty
        , prettyClass [name, "extends", "AnyFreeSpec", "with", "should.Matchers"] $
            vsep $
                [ hsep ["given", "CurrentPrefix", "=", "new", "CurrentPrefix:"]
                , indent 2 $ hsep ["override", "def", "prefix:", "String", "=", "\"Test\""]
                , mempty
                ]
                    ++ map (line <>) tests
        ]
  where
