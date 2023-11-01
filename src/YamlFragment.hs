{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module YamlFragment where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (FromJSON, Parser, Value (..), parseJSON, (.:))
import Prettyprinter (Pretty (..), indent, line, nest, vsep, (<+>))
import Prettyprinter.Internal.Type (Doc)

data YamlFragment = YamlFragment
    { fragmentName :: Text
    , fragmentType :: FragmentType
    , code :: [Text]
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

prettyPrintClass :: Text -> [Text] -> [Doc ann]
prettyPrintClass name body =
    ["class" <+> pretty name <+> "{"]
        ++ map (indent 4 . pretty) body
        ++ ["}"]

prettyPrintFrag :: [Doc ann] -> Doc ann
prettyPrintFrag code =
    vsep $
        ["frag("]
            ++ [")"]

instance Pretty YamlFragment where
    pretty :: YamlFragment -> Doc ann
    pretty frag = case fragmentType frag of
        _ -> vsep $ prettyPrintClass (fragmentName frag) (code frag)
