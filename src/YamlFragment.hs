{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module YamlFragment where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (FromJSON, Parser, Value (..), parseJSON, (.:))
import Prettyprinter (Pretty (..), hcat, hsep, indent, line, nest, vsep, (<+>))
import Prettyprinter.Internal.Type (Doc)

data YamlFragment = YamlFragment
    { fragmentName :: Text
    , fragmentType :: FragmentType
    , fragmentCode :: [Text]
    }
    deriving (Show)

data FragmentType = Test | Definition | Shred deriving (Show)

instance FromJSON FragmentType where
    parseJSON :: Value -> Parser FragmentType
    parseJSON (String s) = case s of
        "Test" -> pure Test
        "Definition" -> pure Definition
        "Shred" -> pure Shred
        _ -> fail $ Text.unpack s ++ " is not a valid fragment type."

instance FromJSON YamlFragment where
    parseJSON :: Value -> Parser YamlFragment
    parseJSON (Object v) =
        YamlFragment
            <$> v
            .: "piece"
            <*> v
            .: "type"
            <*> fmap Text.lines (v .: "code")

prettyClass :: Text -> [Text] -> [Doc ann]
prettyClass name body =
    ["class" <+> pretty name <+> "{"]
        ++ map (indent 2 . pretty) body
        ++ ["}"]

prettyFrag :: [Doc ann] -> Doc ann
prettyFrag code =
    hcat
        [ "frag("
        , line
        , indent 2 "\"\"\""
        , ( case code of
                [x] -> x
                x : xs -> x <> line <> vsep (map (indent 4 . ("|" <>)) xs)
                [] -> ""
          )
        , "\"\"\".stripMargin"
        , line
        , ")"
        ]

prettyPieces :: [Doc ann] -> Doc ann
prettyPieces frags =
    hsep ["val", "pieces", "=", "List("]
        <> nest 2 (hcat (zipWith (<>) ("\n" : repeat ",\n") frags))
        <> line
        <> ")"

instance Pretty YamlFragment where
    pretty :: YamlFragment -> Doc ann
    pretty frag = prettyFrag $
        case fragmentType frag of
            Shred -> prettyClass (fragmentName frag) (fragmentCode frag)
            _ -> map pretty $ fragmentCode frag
