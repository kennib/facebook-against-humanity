{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Facebook.JSON where

import Data.Char (toLower)
import Data.Aeson.TH
import Facebook.Types

$(deriveJSON (defaultOptions { fieldLabelModifier = drop (length ("result" :: String)), constructorTagModifier = map toLower }) ''Result)
$(deriveJSON defaultOptions ''Paging)
$(deriveJSON defaultOptions ''Post)
$(deriveJSON defaultOptions ''Person)
