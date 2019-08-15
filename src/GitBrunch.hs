{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module GitBrunch where


import           Control.Monad                            ( void )
import           Data.Maybe                               ( fromMaybe )
import           Data.Monoid
import           Debug.Trace
import qualified Graphics.Vty                  as V
import           Lens.Micro                               ( (^.) )

import qualified Brick.AttrMap                 as A
import qualified Brick.Main                    as M
import           Brick.Types                              ( Widget )
import qualified Brick.Types                   as T
import           Brick.Util                               ( fg
                                                          , on
                                                          )
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core                       ( hLimit
                                                          , str
                                                          , vBox
                                                          , hBox
                                                          , vLimit
                                                          , padLeft
                                                          , withAttr
                                                          , padRight
                                                          , withBorderStyle
                                                          , (<+>)
                                                          , padAll
                                                          )
import qualified Brick.Widgets.List            as L
import qualified Data.Vector                   as Vec
import           Git
import           Data.Maybe                    as Maybe
import           Data.List
import           Data.Char

data Name = Local | Remote deriving (Ord, Eq, Show)
data State = State { focus :: Name, localBranches :: L.List Name Branch, remoteBranches :: L.List Name Branch }

drawUI :: State -> [Widget Name]
drawUI state =
  [ C.vCenter $ padAll 1 $ vBox
      [ hBox [C.hCenter localBranchList, C.hCenter remoteBranchList]
      , str " "
      , instructions
      ]
  ]
 where
  hasFocus l = L.listName l == focus state
  localBranchList =
    drawBranchList (hasFocus $ localBranches state) (localBranches state)
  remoteBranchList =
    drawBranchList (hasFocus $ remoteBranches state) (remoteBranches state)
  instructions = vBox
    [ drawInstruction "HJKL/arrows" "navigate"
    , drawInstruction "Enter"       "checkout"
    , drawInstruction "Esc/Q"       "exit"
    ]

drawBranchList :: Show a => Bool -> L.List Name a -> Widget Name
drawBranchList hasFocus list =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (drawTitle list)
    $ hLimit 80
    $ L.renderList listDrawElement hasFocus list
 where
  title Local  = map toUpper "local"
  title Remote = map toUpper "remote"
  drawTitle = withAttr "bold" . str . title . L.listName

listDrawElement :: Show a => Bool -> a -> Widget Name
listDrawElement selected a = padLeft (T.Pad 1) $ padRight T.Max $ str (show a)

drawInstruction keys action =
  C.hCenter
    $   str "Press "
    <+> withAttr "key" (str keys)
    <+> str " to "
    <+> withAttr "bold" (str action)
    <+> str "."

appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent state (T.VtyEvent e) =
  let
    checkoutBranch = M.halt state
    focusLocal     = M.continue $ state { focus = Local }
    focusRemote    = M.continue $ state { focus = Remote }

    quit = M.halt $ state { localBranches = L.listClear $ localBranches state }
    navigateDefault event = case focus state of
      Local ->
        (M.continue . (\l -> state { localBranches = l }))
          =<< L.handleListEventVi L.handleListEvent event (localBranches state)
      Remote ->
        (M.continue . (\l -> state { remoteBranches = l }))
          =<< L.handleListEventVi L.handleListEvent event (remoteBranches state)
  in
    case e of
      V.EvKey V.KEsc        [] -> quit
      V.EvKey (V.KChar 'q') [] -> quit
      V.EvKey V.KEnter      [] -> checkoutBranch
      V.EvKey V.KLeft       [] -> focusLocal
      V.EvKey (V.KChar 'h') [] -> focusLocal
      V.EvKey V.KRight      [] -> focusRemote
      V.EvKey (V.KChar 'l') [] -> focusRemote
      event                    -> navigateDefault event
appEvent state _ = M.continue state


attributeMap :: A.AttrMap
attributeMap = A.attrMap
  V.defAttr
  [ (L.listAttr               , fg V.white)
  , (L.listSelectedAttr       , fg V.white)
  , (L.listSelectedFocusedAttr, V.black `on` V.yellow)
  , (A.attrName "key", V.withStyle (V.brightYellow `on` V.black) V.bold)
  , (A.attrName "bold"        , V.withStyle (fg V.white) V.bold)
  ]

initialState :: [Branch] -> State
initialState branches = State
  { focus          = Local
  , localBranches  = L.list Local (Vec.fromList local) 1
  , remoteBranches = L.list Remote (Vec.fromList remote) 1
  }
 where
  (remote, local) = partition isRemote branches
  isRemote (BranchRemote _ _) = True
  isRemote _                  = False

theApp :: M.App State e Name
theApp = M.App { M.appDraw         = drawUI
               , M.appChooseCursor = M.showFirstCursor
               , M.appHandleEvent  = appEvent
               , M.appStartEvent   = return
               , M.appAttrMap      = const attributeMap
               }
stateToBranch :: State -> Maybe Branch
stateToBranch State { focus, localBranches, remoteBranches } = snd
  <$> L.listSelectedElement bs
 where
  bs = case focus of
    Local  -> localBranches
    Remote -> remoteBranches



main :: IO ()
main = do
  branches   <- Git.listBranches
  finalState <- M.defaultMain theApp (initialState branches)
  printResult
    =<< (case stateToBranch finalState of
          Just branch -> Git.checkout branch
          Nothing     -> pure $ Left "No branch selected."
        )
 where
  printResult (Left  e  ) = putStr e
  printResult (Right msg) = putStr msg

