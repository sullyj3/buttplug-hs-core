module JSONUtils where

import Data.Aeson (Options(..), defaultOptions, JSONKeyOptions(..), defaultJSONKeyOptions)
import Data.Aeson.Casing

pascalCaseOptions :: Options
pascalCaseOptions = defaultOptions { fieldLabelModifier = pascalCase }

pascalCaseKeyOptions :: JSONKeyOptions
pascalCaseKeyOptions = defaultJSONKeyOptions { keyModifier = pascalCase }
