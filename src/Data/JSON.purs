module Data.JSON where

import Foreign (Foreign)

foreign import stringify :: Foreign -> String
