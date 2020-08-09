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
                                                , (%~)
                                                , (&)
                                                , Lens'
                                                , lens
                                                )
import           System.Exit
import qualified Brick.Main                    as M
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.List            as L
import qualified Brick.Widgets.Edit            as E
import qualified Data.Vector                   as Vec

import           Git
import           Theme


data Name         = Local | Remote | Filter deriving (Ord, Eq, Show)
data RemoteName   = RLocal | RRemote deriving (Eq)
data GitCommand   = GitRebase | GitCheckout | GitDeleteBranch deriving (Ord, Eq)
data DialogResult = SetDialog Dialog | EndDialog DialogOption
data DialogOption = Cancel | Confirm
type Dialog = D.Dialog DialogOption

data State = State
  { _focus :: RemoteName
  , _gitCommand :: GitCommand
  , _branches :: [Branch]
  , _localBranches :: L.List Name Branch
  , _remoteBranches :: L.List Name Branch
  , _dialog :: Maybe Dialog
  , _filter :: E.Editor String Name
  , _isEditingFilter :: Bool
  }


instance (Show GitCommand) where
  show GitCheckout     = "checkout"
  show GitRebase       = "rebase"
  show GitDeleteBranch = "delete"


main :: IO ()
main = do
  branches <- Git.listBranches `catch` gitFailed
  state <- M.defaultMain app $ updateLists emptyState { _branches = branches }
  let execGit = gitFunction (_gitCommand state)
  exitCode <- maybe noBranchErr execGit (selectedBranch state)
  when (exitCode /= ExitSuccess)
    $ die ("Failed to " ++ show (_gitCommand state) ++ ".")
 where
  gitFailed :: SomeException -> IO a
  gitFailed _ = exitFailure
  noBranchErr = die "No branch selected."
  gitFunction = \case
    GitCheckout     -> Git.checkout
    GitRebase       -> Git.rebaseInteractive
    GitDeleteBranch -> Git.deleteBranch

emptyState :: State
emptyState =
  let mkList focus = L.list focus Vec.empty 1
  in  State { _focus           = RLocal
            , _gitCommand      = GitCheckout
            , _branches        = []
            , _localBranches   = mkList Local
            , _remoteBranches  = mkList Remote
            , _dialog          = Nothing
            , _filter          = emptyFilter
            , _isEditingFilter = False
            }

emptyFilter :: E.Editor String Name
emptyFilter = E.editor Filter Nothing ""

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
          (maxWidth 200 <$> [branchLists, filterEdit, padding, instructions])
      ]
 where
  padding = str " "
  maxWidth w = C.hCenter . hLimit w
  toBranchList r lens' = state ^. lens' & drawBranchList (state ^. focusL == r)
  filterEdit  = if _isEditingFilter state then drawFilter state else emptyWidget
  branchLists = hBox
    [ C.hCenter $ toBranchList RLocal localBranchesL
    , str " "
    , C.hCenter $ toBranchList RRemote remoteBranchesL
    ]
  instructions = maxWidth 100 $ hBox
    [ drawInstruction "HJKL"  "move"
    , drawInstruction "Enter" "checkout"
    , drawInstruction "/"     "filter"
    , drawInstruction "F"     "fetch"
    , drawInstruction "R"     "rebase"
    , drawInstruction "D"     "delete"
    ]

drawFilter :: State -> Widget Name
drawFilter state =
  withBorderStyle BS.unicodeBold
    $   B.border
    $   padLeft (Pad 1)
    $   vLimit 1
    $   label
    <+> editor
 where
  editor = E.renderEditor (str . unlines) True (state ^. filterL)
  label  = str "Filter: "

drawDialog :: State -> Widget n
drawDialog state = case _dialog state of
  Nothing     -> emptyWidget
  Just dialog -> D.renderDialog dialog $ C.hCenter $ padAll 1 content
   where
    branch = maybe "" show $ selectedBranch state
    action = show (_gitCommand state)
    content =
      str "Really "
        <+> withAttr attrUnder (str action)
        <+> str " branch "
        <+> withAttr attrBold (str branch)
        <+> str "?"

drawBranchList :: Bool -> L.List Name Branch -> Widget Name
drawBranchList hasFocus list =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (drawTitle list)
    $ L.renderList drawListElement hasFocus list
 where
  attr      = withAttr $ if hasFocus then attrTitleFocus else attrTitle
  drawTitle = attr . str . map toUpper . show . L.listName

drawListElement :: Bool -> Branch -> Widget Name
drawListElement _ branch =
  padLeft (Pad 1) $ padRight Max $ highlight branch $ str $ show branch
 where
  highlight (BranchCurrent _)    = withAttr attrBranchCurrent
  highlight b | isCommonBranch b = withAttr attrBranchCommon
  highlight _                    = id

drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  withAttr attrKey (str keys)
    <+> str " to "
    <+> withAttr attrBold (str action)
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

appHandleEventDialog :: Dialog -> BrickEvent Name e -> EventM Name DialogResult
appHandleEventDialog dialog (VtyEvent e) =
  let quit         = pure $ EndDialog Cancel
      closeDialog  = pure $ EndDialog Cancel
      dialogAction = pure $ case D.dialogSelection dialog of
        Just Cancel  -> EndDialog Cancel
        Just confirm -> EndDialog confirm
        Nothing      -> SetDialog dialog
  in  case vimKey $ lowerKey e of
        EvKey (KChar 'c') [MCtrl] -> quit
        EvKey (KChar 'd') [MCtrl] -> quit
        EvKey KEnter      []      -> dialogAction
        EvKey KEsc        []      -> closeDialog
        EvKey (KChar 'q') []      -> closeDialog
        ev                        -> SetDialog <$> D.handleDialogEvent ev dialog
appHandleEventDialog dialog _ = pure $ SetDialog dialog

appHandleEventMain :: State -> BrickEvent Name e -> EventM Name (Next State)
appHandleEventMain state (VtyEvent e) =
  let
    confirm c = state { _gitCommand = c, _dialog = Just $ createDialog c }
    confirmDelete (Just (BranchCurrent _)) = continue state
    confirmDelete _                        = continue $ confirm GitDeleteBranch
    deleteSelection    = focussedBranchesL %~ L.listClear
    endWithCheckout    = halt $ state { _gitCommand = GitCheckout }
    endWithRebase      = halt $ state { _gitCommand = GitRebase }
    focusLocal         = focusBranches RLocal state
    focusRemote        = focusBranches RRemote state
    quit               = halt $ deleteSelection state
    doFetch            = suspendAndResume (fetchBranches state)
    startEditingFilter = continue $ updateLists $ state
      { _isEditingFilter = True
      , _filter          = emptyFilter
      }
    cancelEditingFilter = continue $ updateLists $ state
      { _isEditingFilter = False
      , _filter          = emptyFilter
      }
    stopEditingFilter =
      continue $ updateLists $ state { _isEditingFilter = False }
    handle =
      if _isEditingFilter state then handleEditingFilter else handleDefault
    handleDefault = \case
      EvKey (KChar 'c') [MCtrl] -> quit
      EvKey (KChar 'd') [MCtrl] -> quit
      EvKey KEsc        []      -> quit
      EvKey (KChar 'q') []      -> quit
      EvKey (KChar '/') []      -> startEditingFilter
      EvKey (KChar 'f') [MCtrl] -> startEditingFilter
      EvKey (KChar 'd') []      -> confirmDelete (selectedBranch state)
      EvKey KEnter      []      -> endWithCheckout
      EvKey (KChar 'r') []      -> endWithRebase
      EvKey KLeft       []      -> focusLocal
      EvKey (KChar 'h') []      -> focusLocal
      EvKey KRight      []      -> focusRemote
      EvKey (KChar 'l') []      -> focusRemote
      EvKey (KChar 'f') []      -> doFetch
      _                         -> navigate state e
    handleEditingFilter = \case
      EvKey (KChar 'c') [MCtrl] -> quit
      EvKey (KChar 'd') [MCtrl] -> quit
      EvKey KEsc        []      -> cancelEditingFilter
      EvKey KEnter      []      -> stopEditingFilter
      EvKey KUp         []      -> stopEditingFilter
      EvKey KDown       []      -> stopEditingFilter
      _                         -> (updateLists <$>) <$> handleFilter state e
  in
    handle $ lowerKey e

appHandleEventMain state _ = continue state


navigate :: State -> Event -> EventM Name (Next State)
navigate state event = do
  let update = L.handleListEventVi L.handleListEvent
  newState <- handleEventLensed state focussedBranchesL update event
  continue newState

handleFilter :: State -> Event -> EventM Name (Next State)
handleFilter state event = do
  newState <- handleEventLensed state filterL E.handleEditorEvent event
  continue newState

focusBranches :: RemoteName -> State -> EventM Name (Next State)
focusBranches target state = if isAlreadySelected
  then continue state
  else do
    offsetDiff <- listOffsetDiff target
    continue $ state & changeList & syncPosition offsetDiff
 where
  isAlreadySelected = state ^. focusL == target
  changeList        = focusL .~ target
  listIndex         = fromMaybe 0 $ state ^. currentListL . L.listSelectedL
  syncPosition diff = targetListL %~ L.listMoveTo (listIndex - diff)
  (currentListL, targetListL) = case target of
    RLocal  -> (remoteBranchesL, localBranchesL)
    RRemote -> (localBranchesL, remoteBranchesL)

listOffsetDiff :: RemoteName -> EventM Name Int
listOffsetDiff target = do
  offLocal  <- getOffset Local
  offRemote <- getOffset Remote
  return
    $ if target == RLocal then offRemote - offLocal else offLocal - offRemote
  where getOffset name = maybe 0 (^. vpTop) <$> M.lookupViewport name

fetchBranches :: State -> IO State
fetchBranches state = do
  putStrLn "Fetching branches"
  output <- fetch
  putStr output
  branches <- listBranches
  return $ updateLists state { _branches = branches }

updateLists :: State -> State
updateLists state = newState
 where
  mkList n xs = L.list n (Vec.fromList xs) 1
  isRemote (BranchRemote _ _) = True
  isRemote _                  = False
  filterString = unwords $ E.getEditContents $ _filter state
  filteredBranches =
    filter (isInfixOf filterString . fullBranchName) (_branches state)
  (remote, local) = partition isRemote filteredBranches
  newState        = state { _localBranches  = mkList Local local
                          , _remoteBranches = mkList Remote remote
                          }


selectedBranch :: State -> Maybe Branch
selectedBranch state =
  snd <$> L.listSelectedElement (state ^. focussedBranchesL)

createDialog :: GitCommand -> Dialog
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
        RLocal  -> localBranchesL
        RRemote -> remoteBranchesL
  in  lens (\s -> s ^. branchLens s) (\s bs -> (branchLens s .~ bs) s)

localBranchesL :: Lens' State (L.List Name Branch)
localBranchesL = lens _localBranches (\s bs -> s { _localBranches = bs })

remoteBranchesL :: Lens' State (L.List Name Branch)
remoteBranchesL = lens _remoteBranches (\s bs -> s { _remoteBranches = bs })

focusL :: Lens' State RemoteName
focusL = lens _focus (\s f -> s { _focus = f })

filterL :: Lens' State (E.Editor String Name)
filterL = lens _filter (\s f -> s { _filter = f })
