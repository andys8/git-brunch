{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module GitBrunch
  ( main
  )
where

import           Brick.Main                     ( halt
                                                , continue
                                                , suspendAndResume
                                                )
import           Brick.Themes                   ( themeToAttrMap )
import           Brick.Types
import           Brick.Widgets.Core
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe                     ( fromMaybe )
import           Graphics.Vty            hiding ( update )
import           Lens.Micro                     ( (^.)
                                                , (.~)
                                                , (&)
                                                , Lens'
                                                , lens
                                                , over
                                                )
import           System.Exit
import qualified Brick.Main                    as M
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.List            as L
import qualified Data.Vector                   as Vec

import           Git
import           Theme


data Name = Local | Remote deriving (Ord, Eq, Show)
data GitCommand = GitRebase | GitCheckout | GitDeleteBranch deriving (Ord, Eq)

data DialogOption = Cancel | Confirm
data DialogResult = SetDialog (D.Dialog DialogOption)
                  | EndDialog DialogOption

data State = State
  { _focus :: Name
  , _gitCommand :: GitCommand
  , _localBranches :: L.List Name Branch
  , _remoteBranches :: L.List Name Branch
  , _dialog :: Maybe (D.Dialog DialogOption)
  }

instance (Show GitCommand) where
  show GitCheckout     = "checkout"
  show GitRebase       = "rebase"
  show GitDeleteBranch = "delete"

main :: IO ()
main = do
  branches <- Git.listBranches `catch` gitFailed
  state    <- M.defaultMain app $ setBranches branches initialState
  let gitCommand = _gitCommand state
  let branch     = selectedBranch state
  let runGit = \case
        GitCheckout     -> Git.checkout
        GitRebase       -> Git.rebaseInteractive
        GitDeleteBranch -> Git.deleteBranch
  exitCode <- maybe (die "No branch selected.") (runGit gitCommand) branch
  when (exitCode /= ExitSuccess) $ die ("Failed to " ++ show gitCommand ++ ".")
 where
  gitFailed :: SomeException -> IO a
  gitFailed _ = exitFailure

initialState :: State
initialState = State Local GitCheckout (toList Local) (toList Remote) Nothing
  where toList n = L.list n Vec.empty 1

app :: M.App State e Name
app = M.App { M.appDraw         = appDraw
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent  = appHandleEvent
            , M.appStartEvent   = return
            , M.appAttrMap      = const $ themeToAttrMap theme
            }

appDraw :: State -> [Widget Name]
appDraw state =
  drawDialog state
    : [ C.vCenter $ padAll 1 $ vBox
          [ hBox
            [ C.hCenter $ toBranchList localBranchesL
            , C.hCenter $ toBranchList remoteBranchesL
            ]
          , str " "
          , instructions
          ]
      ]
 where
  toBranchList lens' = state ^. lens' & (\l -> drawBranchList (hasFocus l) l)
  hasFocus     = (_focus state ==) . L.listName
  instructions = C.hCenter $ hLimit 100 $ hBox
    [ drawInstruction "HJKL"  "navigate"
    , drawInstruction "Enter" "checkout"
    , drawInstruction "R"     "rebase"
    , drawInstruction "F"     "fetch"
    , drawInstruction "D"     "delete"
    ]

drawDialog :: State -> Widget n
drawDialog state = case _dialog state of
  Nothing     -> emptyWidget
  Just dialog -> D.renderDialog dialog $ C.hCenter $ padAll 1 content
   where
    branch = maybe "" show $ selectedBranch state
    action = show (_gitCommand state)
    content =
      str "Really "
        <+> withAttr "under" (str action)
        <+> str " branch "
        <+> withAttr "bold" (str branch)
        <+> str "?"

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
  padLeft (Pad 1) $ padRight Max $ highlight branch $ str $ show branch
 where
  highlight (BranchCurrent _) = withAttr "current"
  highlight _                 = id

drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  withAttr "key" (str keys)
    <+> str " to "
    <+> withAttr "bold" (str action)
    &   C.hCenter

appHandleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
appHandleEvent state e = case _dialog state of
  Nothing -> appHandleEventMain state e
  Just d  -> toState =<< appHandleEventDialog d e
   where
    toState (SetDialog dlg    ) = continue $ state { _dialog = Just dlg }
    toState (EndDialog Confirm) = halt $ state { _dialog = Nothing }
    toState (EndDialog Cancel) =
      continue $ state { _dialog = Nothing, _gitCommand = GitCheckout }

appHandleEventMain :: State -> BrickEvent Name e -> EventM Name (Next State)
appHandleEventMain state (VtyEvent e) =
  let confirm c = state { _gitCommand = c, _dialog = Just $ createDialog c }
      confirmDelete (Just (BranchCurrent _)) = continue state
      confirmDelete _ = continue $ confirm GitDeleteBranch
      deleteSelection = focussedBranchesL `over` L.listClear
      endWithCheckout = halt $ state { _gitCommand = GitCheckout }
      endWithRebase   = halt $ state { _gitCommand = GitRebase }
      focusLocal      = continue $ focusBranches Local state
      focusRemote     = continue $ focusBranches Remote state
      quit            = halt $ deleteSelection state
      updateBranches  = suspendAndResume (updateBranchList state)
  in  case lowerKey e of
        EvKey (KChar 'c') [MCtrl] -> quit
        EvKey (KChar 'd') [MCtrl] -> quit
        EvKey KEsc        []      -> quit
        EvKey (KChar 'q') []      -> quit
        EvKey (KChar 'd') []      -> confirmDelete (selectedBranch state)
        EvKey KEnter      []      -> endWithCheckout
        EvKey (KChar 'r') []      -> endWithRebase
        EvKey KLeft       []      -> focusLocal
        EvKey (KChar 'h') []      -> focusLocal
        EvKey KRight      []      -> focusRemote
        EvKey (KChar 'l') []      -> focusRemote
        EvKey (KChar 'f') []      -> updateBranches
        _                         -> navigate state e
appHandleEventMain state _ = continue state

appHandleEventDialog
  :: D.Dialog DialogOption -> BrickEvent Name e -> EventM Name DialogResult
appHandleEventDialog dialog (VtyEvent e) =
  let quit         = pure $ EndDialog Cancel
      closeDialog  = pure $ EndDialog Cancel
      dialogAction = pure $ case D.dialogSelection dialog of
        Just Cancel  -> EndDialog Cancel
        Just confirm -> EndDialog confirm
        Nothing      -> SetDialog dialog
  in  case vimKey $ lowerKey e of
        EvKey KEnter      []      -> dialogAction
        EvKey (KChar 'c') [MCtrl] -> quit
        EvKey (KChar 'd') [MCtrl] -> quit
        EvKey KEsc        []      -> closeDialog
        EvKey (KChar 'q') []      -> closeDialog
        ev                        -> SetDialog <$> D.handleDialogEvent ev dialog
appHandleEventDialog dialog _ = pure $ SetDialog dialog

navigate :: State -> Event -> EventM Name (Next State)
navigate state event = do
  let update = L.handleListEventVi L.handleListEvent
  newState <- handleEventLensed state focussedBranchesL update event
  continue newState

updateBranchList :: State -> IO State
updateBranchList state = do
  putStrLn "Fetching branches"
  output <- fetch
  putStr output
  branches <- listBranches
  return $ setBranches branches state

focusBranches :: Name -> State -> State
focusBranches target state = if state ^. focusL == target
  then state
  else state & toL `over` L.listMoveTo selectedIndex & focusL .~ target
 where
  selectedIndex = fromMaybe 0 $ L.listSelected (state ^. fromL)
  (fromL, toL)  = case target of
    Local  -> (remoteBranchesL, localBranchesL)
    Remote -> (localBranchesL, remoteBranchesL)

setBranches :: [Branch] -> State -> State
setBranches branches state = newState
 where
  (remote, local) = partition isRemote branches
  isRemote (BranchRemote _ _) = True
  isRemote _                  = False
  toList n xs = L.list n (Vec.fromList xs) 1
  newState = state { _localBranches  = toList Local local
                   , _remoteBranches = toList Remote remote
                   }

selectedBranch :: State -> Maybe Branch
selectedBranch state =
  snd <$> L.listSelectedElement (state ^. focussedBranchesL)

createDialog :: GitCommand -> D.Dialog DialogOption
createDialog cmd = D.dialog (Just title) (Just (0, choices)) 80
 where
  choices = [(btnText $ show cmd, Confirm), ("Cancel", Cancel)]
  title   = map toUpper $ show cmd
  btnText (x : xs) = toUpper x : xs
  btnText x        = x

mapKey :: (Char -> Key) -> Event -> Event
mapKey f (EvKey (KChar k) []) = EvKey (f k) []
mapKey _ e                    = e

lowerKey :: Event -> Event
lowerKey = mapKey (KChar . toLower)

vimKey :: Event -> Event
vimKey = mapKey vimify
 where
  vimify 'h' = KLeft
  vimify 'j' = KRight
  vimify 'k' = KLeft
  vimify 'l' = KRight
  vimify k   = KChar k


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

