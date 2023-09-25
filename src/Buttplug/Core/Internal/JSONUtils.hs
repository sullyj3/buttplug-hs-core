module Buttplug.Core.Internal.JSONUtils where

import Data.Aeson (Options(..), defaultOptions, JSONKeyOptions(..), defaultJSONKeyOptions)
import Data.Aeson.Casing

-- TODO some of the uses of pascalCase are probably redundant

stripPrefix :: String -> String -> String
stripPrefix s = drop $ length s

pascalCaseKeyOptions :: JSONKeyOptions
pascalCaseKeyOptions = defaultJSONKeyOptions { keyModifier = pascalCase }

deviceMessageOptions :: Options
deviceMessageOptions =
  defaultOptions { constructorTagModifier = stripPrefix "Dev" }

msgAttributeOptions :: Options
msgAttributeOptions =
  defaultOptions { omitNothingFields = True
                 , fieldLabelModifier = pascalCase . stripPrefix "attr"}
