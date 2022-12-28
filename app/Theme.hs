module Theme where

import Brick.AttrMap (AttrName, attrName)
import Brick.Themes
import Brick.Util
import Brick.Widgets.Border as Border
import Brick.Widgets.Dialog qualified as Dialog
import Brick.Widgets.Edit qualified as Edit
import Brick.Widgets.List qualified as List
import Graphics.Vty

theme :: Theme
theme =
  newTheme
    (white `on` brightBlack)
    [ (List.listAttr, fg brightWhite)
    , (List.listSelectedAttr, fg brightWhite)
    , (List.listSelectedFocusedAttr, black `on` brightYellow)
    , (Dialog.dialogAttr, fg brightWhite)
    , (Dialog.buttonAttr, brightBlack `on` white)
    , (Dialog.buttonSelectedAttr, black `on` brightMagenta)
    , (Border.borderAttr, fg white)
    , (Edit.editFocusedAttr, fg brightWhite)
    , (attrKey, withStyle (fg brightMagenta) bold)
    , (attrBold, withStyle (fg white) bold)
    , (attrUnder, withStyle (fg brightWhite) underline)
    , (attrTitle, withStyle (fg brightWhite) bold)
    , (attrTitleFocus, withStyle (fg yellow) bold)
    , (attrBranchCurrent, fg brightRed)
    , (attrBranchCommon, fg brightBlue)
    ]

attrKey :: AttrName
attrKey = attrName "key"

attrBold :: AttrName
attrBold = attrName "bold"

attrUnder :: AttrName
attrUnder = attrName "under"

attrTitle :: AttrName
attrTitle = attrName "title"

attrTitleFocus :: AttrName
attrTitleFocus = attrName "title-focus"

attrBranchCurrent :: AttrName
attrBranchCurrent = attrName "current-branch"

attrBranchCommon :: AttrName
attrBranchCommon = attrName "common-branch"
