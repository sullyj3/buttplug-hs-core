module Buttplug.Internal.JSONUtils where

import Data.Aeson (Options(..), defaultOptions, JSONKeyOptions(..), defaultJSONKeyOptions)
import Data.Aeson.Casing

stripPrefix :: String -> String -> String
stripPrefix s = drop $ length s

stripPrefixOptions s = defaultOptions { fieldLabelModifier = stripPrefix s }

pascalCaseOptions :: Options
pascalCaseOptions = defaultOptions { fieldLabelModifier = pascalCase }

pascalCaseKeyOptions :: JSONKeyOptions
pascalCaseKeyOptions = defaultJSONKeyOptions { keyModifier = pascalCase }

msgAttributeOptions :: Options
msgAttributeOptions = 
  defaultOptions { omitNothingFields = True
                 , fieldLabelModifier = pascalCase . stripPrefix "attr"}
