{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module YamlFragment where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (FromJSON, Parser, Value (..), parseJSON, (.:))
import Prettyprinter (Pretty (..), hcat, indent, vsep)
import Prettyprinter.Internal.Type (Doc)
import TestGen (prettyClass)

data YamlFragment = YamlFragment
    { fragmentName :: Text
    , fragmentType :: FragmentType
    , fragmentCode :: Text
    }

data FragmentType = Test | Definition | Shred

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
            <*> v
            .: "code"

instance Pretty YamlFragment where
    pretty :: YamlFragment -> Doc ann
    pretty frag = prettyFrag $
        case fragmentType frag of
            Shred -> prettyClass [fragmentName frag] (pretty $ fragmentCode frag)
            _ -> pretty $ fragmentCode frag
      where
        prettyFrag :: Doc ann -> Doc ann
        prettyFrag code =
            vsep
                [ "frag("
                , indent 2 "\"\"\""
                    <> ( case code_split of
                            [x] -> x
                            x : xs -> vsep $ x : (map (indent 4 . ("|" <>)) xs)
                            [] -> ""
                       )
                    <> "\"\"\".stripMargin"
                , ")"
                ]
          where
            code_split = map pretty $ lines $ show code
