module Theme
  ( theme
  )
where

import           Graphics.Vty
import qualified Brick.Widgets.List            as List
import           Brick.AttrMap                            ( attrName )
import           Brick.Util
import           Brick.Themes                             ( Theme
                                                          , newTheme
                                                          )

theme :: Theme
theme = newTheme
  (white `on` brightBlack)
  [ (List.listAttr               , fg brightWhite)
  , (List.listSelectedAttr       , fg brightWhite)
  , (List.listSelectedFocusedAttr, black `on` brightYellow)
  , (attrName "key"              , withStyle (fg brightMagenta) bold)
  , (attrName "bold"             , withStyle (fg white) bold)
  , (attrName "current"          , fg brightRed)
  , (attrName "title"            , withStyle (fg yellow) bold)
  ]

