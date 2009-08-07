module Text.Document.PluginRegister (pluginRegister) where

import Text.Document.Plugin
import Text.Document.Core.Type

import Text.Document.Plugin.HsColour
import Text.Document.Plugin.Formula
import Text.Document.Plugin.TOC

pluginRegister :: [Plugin Document Syn_Document]
pluginRegister = [
    formulaPlugin
  , hscolourPlugin
  , tocPlugin
  ]

