{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module GitBrunch
  ( main
  )
where

import           Data.Maybe                               ( fromMaybe )
import qualified Graphics.Vty                  as V
import           Lens.Micro                               ( (^.) -- view
                                                          , (.~) -- set
                                                          , (%~) -- over
                                                          , (&)
                                                          , Lens'
                                                          , lens
                                                          )

import qualified Brick.Main                    as M
import           Brick.Types                              ( Widget )
import           Brick.Themes                             ( themeToAttrMap )
import qualified Brick.Types                   as T
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core                       ( hLimit
                                                          , str
                                                          , vBox
                                                          , hBox
                                                          , padLeft
                                                          , withAttr
                                                          , padRight
                                                          , withBorderStyle
                                                          , (<+>)
                                                          , padAll
                                                          )
import qualified Brick.Widgets.List            as L
import qualified Data.Vector                   as Vec
import           Data.List
import           Data.Char

import           Git
import           Theme                                    ( theme )


data Name = Local | Remote deriving (Ord, Eq, Show)
data State = State { _focus :: Name, _localBranches :: L.List Name Branch, _remoteBranches :: L.List Name Branch }


main :: IO ()
main = do
  branches   <- Git.listBranches
  finalState <- M.defaultMain app (initialState branches)
  printResult =<< checkoutBranch (selectedBranch finalState)
 where
  printResult (Left  err) = putStr err
  printResult (Right msg) = putStr msg
  checkoutBranch (Just b) = Git.checkout b
  checkoutBranch Nothing  = pure $ Left "No branch selected."


app :: M.App State e Name
app = M.App { M.appDraw         = appDraw
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent  = appHandleEvent
            , M.appStartEvent   = return
            , M.appAttrMap      = const $ themeToAttrMap theme
            }


appDraw :: State -> [Widget Name]
appDraw state =
  [ C.vCenter $ padAll 1 $ vBox
      [ hBox
        [ C.hCenter $ toBranchList localBranchesL
        , C.hCenter $ toBranchList remoteBranchesL
        ]
      , str " "
      , vBox
        [ drawInstruction "HJKL/arrows" "navigate"
        , drawInstruction "Enter"       "checkout"
        , drawInstruction "Esc/Q"       "exit"
        ]
      ]
  ]
 where
  toBranchList lens' = state ^. lens' & (\l -> drawBranchList (hasFocus l) l)
  hasFocus = (_focus state ==) . L.listName


drawBranchList :: Bool -> L.List Name Branch -> Widget Name
drawBranchList hasFocus list =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (drawTitle list)
    $ hLimit 80
    $ L.renderList drawListElement hasFocus list
 where
  title Local  = map toUpper "local"
  title Remote = map toUpper "remote"
  drawTitle = withAttr "title" . str . title . L.listName


drawListElement :: Bool -> Branch -> Widget Name
drawListElement _ branch =
  padLeft (T.Pad 1) $ padRight T.Max $ highlight branch $ str $ show branch
 where
  highlight (BranchCurrent _) = withAttr "current"
  highlight _                 = id


drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  C.hCenter
    $   str "Press "
    <+> withAttr "key" (str keys)
    <+> str " to "
    <+> withAttr "bold" (str action)
    <+> str "."


appHandleEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appHandleEvent state (T.VtyEvent e) =
  let checkoutBranch  = M.halt state
      focusLocal      = M.continue $ focusBranches Local state
      focusRemote     = M.continue $ focusBranches Remote state
      deleteSelection = focussedBranchesL %~ L.listClear
      quit            = M.halt $ deleteSelection state
  in  case e of
        V.EvKey V.KEsc        [] -> quit
        V.EvKey (V.KChar 'q') [] -> quit
        V.EvKey V.KEnter      [] -> checkoutBranch
        V.EvKey V.KLeft       [] -> focusLocal
        V.EvKey (V.KChar 'h') [] -> focusLocal
        V.EvKey V.KRight      [] -> focusRemote
        V.EvKey (V.KChar 'l') [] -> focusRemote
        event                    -> navigate state event
appHandleEvent state _ = M.continue state


focusBranches :: Name -> State -> State
focusBranches target state = if state ^. focusL == target
  then state
  else state & toL %~ L.listMoveTo selectedIndex & focusL .~ target
 where
  selectedIndex = fromMaybe 0 $ L.listSelected (state ^. fromL)
  (fromL, toL)  = case target of
    Local  -> (remoteBranchesL, localBranchesL)
    Remote -> (localBranchesL, remoteBranchesL)


navigate :: State -> V.Event -> T.EventM Name (T.Next State)
navigate state event = do
  let update = L.handleListEventVi L.handleListEvent
  newState <- T.handleEventLensed state focussedBranchesL update event
  M.continue newState


initialState :: [Branch] -> State
initialState branches = State
  { _focus          = Local
  , _localBranches  = L.list Local (Vec.fromList local) 1
  , _remoteBranches = L.list Remote (Vec.fromList remote) 1
  }
 where
  (remote, local) = partition isRemote branches
  isRemote (BranchRemote _ _) = True
  isRemote _                  = False


selectedBranch :: State -> Maybe Branch
selectedBranch state =
  snd <$> L.listSelectedElement (state ^. focussedBranchesL)


-- Lens

focussedBranchesL :: Lens' State (L.List Name Branch)
focussedBranchesL =
  let branchLens s = case s ^. focusL of
        Local  -> localBranchesL
        Remote -> remoteBranchesL
  in  lens (\s -> s ^. branchLens s) (\s bs -> (branchLens s .~ bs) s)


localBranchesL :: Lens' State (L.List Name Branch)
localBranchesL = lens _localBranches (\s bs -> s { _localBranches = bs })


remoteBranchesL :: Lens' State (L.List Name Branch)
remoteBranchesL = lens _remoteBranches (\s bs -> s { _remoteBranches = bs })


focusL :: Lens' State Name
focusL = lens _focus (\s f -> s { _focus = f })
