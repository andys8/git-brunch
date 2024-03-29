module GitBrunch (main) where

import Brick.Main (halt)
import Brick.Main qualified as M
import Brick.Themes (themeToAttrMap)
import Brick.Types
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core
import Brick.Widgets.Dialog qualified as D
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Control.Exception (SomeException, catch)
import Control.Monad
import Control.Monad.Extra (ifM, unlessM)
import Data.Char
import Data.List
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as Vec
import Graphics.Vty hiding (update)
import Lens.Micro (Lens', lens, (%~), (&), (.~), (^.), _Just)
import Lens.Micro.Mtl ((%=), (.=), (?=))
import System.Exit

import Git (Branch (..))
import Git qualified
import Theme

data Name
  = Local
  | Remote
  | Filter
  | DialogConfirm
  | DialogCancel
  deriving (Ord, Eq, Show)

data RemoteName
  = RLocal
  | RRemote
  deriving (Eq)

data GitCommand
  = GitRebase
  | GitMerge
  | GitCheckout
  | GitDeleteBranch
  deriving (Ord, Eq)

data DialogOption
  = Cancel
  | Confirm GitCommand

data State = State
  { _focus :: RemoteName
  , _gitCommand :: GitCommand
  , _branches :: [Branch]
  , _localBranches :: L.List Name Branch
  , _remoteBranches :: L.List Name Branch
  , _dialog :: Maybe (D.Dialog DialogOption Name)
  , _filter :: E.Editor Text Name
  , _isEditingFilter :: Bool
  }

instance Show GitCommand where
  show GitCheckout = "checkout"
  show GitRebase = "rebase"
  show GitMerge = "merge"
  show GitDeleteBranch = "delete"

main :: IO ()
main = do
  branches <- Git.listBranches `catch` gitFailed
  state <- M.defaultMain app $ syncBranchLists emptyState{_branches = branches}
  let execGit = gitFunction (_gitCommand state)
  exitCode <- maybe noBranchErr execGit (selectedBranch state)
  when (exitCode /= ExitSuccess)
    $ die ("Failed to " ++ show (_gitCommand state) ++ ".")
 where
  gitFailed :: SomeException -> IO a
  gitFailed _ = exitFailure
  noBranchErr = die "No branch selected."
  gitFunction = \case
    GitCheckout -> Git.checkout
    GitRebase -> Git.rebaseInteractive
    GitMerge -> Git.merge
    GitDeleteBranch -> Git.deleteBranch

emptyState :: State
emptyState =
  State
    { _focus = RLocal
    , _gitCommand = GitCheckout
    , _branches = []
    , _localBranches = mkList Local
    , _remoteBranches = mkList Remote
    , _dialog = Nothing
    , _filter = emptyFilter
    , _isEditingFilter = False
    }
 where
  mkList focus = L.list focus Vec.empty rowHeight

emptyFilter :: E.Editor Text Name
emptyFilter = E.editor Filter Nothing ""

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawApp
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appHandleEvent
    , M.appStartEvent = pure ()
    , M.appAttrMap = const $ themeToAttrMap theme
    }

drawApp :: State -> [Widget Name]
drawApp state =
  drawDialog state : [C.vCenter $ padAll 1 $ maxWidth 200 $ vBox content]
 where
  content = [branchLists, filterEdit, padding, instructions]
  padding = str " "
  maxWidth w = C.hCenter . hLimit w
  toBranchList r lens' =
    let isActive = state ^. focusL == r && not (_isEditingFilter state)
     in state ^. lens' & drawBranchList isActive
  filterEdit = if _isEditingFilter state then drawFilter state else emptyWidget
  branchLists =
    hBox
      [ C.hCenter $ toBranchList RLocal localBranchesL
      , str " "
      , C.hCenter $ toBranchList RRemote remoteBranchesL
      ]
  instructions =
    maxWidth 100
      $ hBox
        [ drawInstruction "Enter" "checkout"
        , drawInstruction "/" "filter"
        , drawInstruction "F" "fetch"
        , drawInstruction "R" "rebase"
        , drawInstruction "M" "merge"
        , drawInstruction "D" "delete"
        ]

drawFilter :: State -> Widget Name
drawFilter state =
  withBorderStyle BS.unicodeBold $ B.border $ vLimit 1 $ label <+> editor
 where
  editor = E.renderEditor (txt . T.unlines) True (state ^. filterL)
  label = str " Filter: "

drawDialog :: State -> Widget Name
drawDialog state = case _dialog state of
  Nothing -> emptyWidget
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
  attr = withAttr $ if hasFocus then attrTitleFocus else attrTitle
  drawTitle = attr . str . map toUpper . show . L.listName

drawListElement :: Bool -> Branch -> Widget Name
drawListElement isListFocussed branch =
  maxPadding $ highlight branch $ str $ " " <> show branch
 where
  maxPadding = if isListFocussed then padRight Max else id
  highlight (BranchCurrent _) = withAttr attrBranchCurrent
  highlight b | Git.isCommonBranch b = withAttr attrBranchCommon
  highlight _ = id

drawInstruction :: Text -> Text -> Widget n
drawInstruction keys action =
  withAttr attrKey (txt keys)
    <+> txt " to "
    <+> withAttr attrBold (txt action)
    & C.hCenter

appHandleEvent :: BrickEvent Name e -> EventM Name State ()
appHandleEvent (VtyEvent e)
  | isQuitEvent e = quit
  | otherwise = do
      dialog <- gets _dialog
      if isJust dialog
        then appHandleEventDialog e
        else appHandleEventMain e
 where
  isQuitEvent (EvKey (KChar 'c') [MCtrl]) = True
  isQuitEvent (EvKey (KChar 'd') [MCtrl]) = True
  isQuitEvent _ = False
appHandleEvent _ = pure ()

appHandleEventMain :: Event -> EventM Name State ()
appHandleEventMain e =
  let
    event = lowerKey e
    endWithCheckout = gitCommandL .= GitCheckout >> halt
    endWithRebase = gitCommandL .= GitRebase >> halt
    endWithMerge = gitCommandL .= GitMerge >> halt
    resetFilter = filterL .~ emptyFilter
    showFilter = isEditingFilterL .~ True
    hideFilter = isEditingFilterL .~ False
    startEditingFilter = modify (showFilter . resetFilter)
    cancelEditingFilter = modify (hideFilter . resetFilter)
    stopEditingFilter = modify hideFilter

    confirmDelete :: Maybe Branch -> EventM Name State ()
    confirmDelete (Just (BranchCurrent _)) = pure ()
    confirmDelete (Just _) = dialogL ?= createDialog GitDeleteBranch
    confirmDelete Nothing = pure ()

    fetch = do
      state <- get
      M.suspendAndResume $ do
        branches <- fetchBranches
        pure $ updateBranches branches state

    handleDefault :: EventM Name State ()
    handleDefault = case event of
      EvKey KEsc [] -> quit
      EvKey (KChar 'q') [] -> quit
      EvKey (KChar '/') [] -> startEditingFilter
      EvKey (KChar 'f') [MCtrl] -> startEditingFilter
      EvKey (KChar 'd') [] -> confirmDelete =<< gets selectedBranch
      EvKey KEnter [] -> endWithCheckout
      EvKey (KChar 'c') [] -> endWithCheckout
      EvKey (KChar 'r') [] -> endWithRebase
      EvKey (KChar 'm') [] -> endWithMerge
      EvKey KLeft [] -> focusBranches RLocal
      EvKey (KChar 'h') [] -> focusBranches RLocal
      EvKey KRight [] -> focusBranches RRemote
      EvKey (KChar 'l') [] -> focusBranches RRemote
      EvKey (KChar 'f') [] -> fetch
      _ -> zoom focussedBranchesL $ L.handleListEventVi L.handleListEvent e

    handleEditingFilter :: EventM Name State ()
    handleEditingFilter = do
      case event of
        EvKey KEsc [] -> cancelEditingFilter
        EvKey KEnter [] -> stopEditingFilter
        EvKey KUp [] -> stopEditingFilter
        EvKey KDown [] -> stopEditingFilter
        _ -> zoom filterL $ E.handleEditorEvent (VtyEvent e)
      modify syncBranchLists
   in
    ifM
      (gets _isEditingFilter)
      handleEditingFilter
      handleDefault

appHandleEventDialog :: Event -> EventM Name State ()
appHandleEventDialog e =
  let
    cancelDialog = do
      dialogL .= Nothing
      gitCommandL .= GitCheckout

    confirmDialog cmd = do
      dialogL .= Nothing
      gitCommandL .= cmd
      halt
   in
    case vimifiedKey e of
      EvKey KEnter [] -> do
        dialog <- gets _dialog
        case D.dialogSelection =<< dialog of
          Just (DialogConfirm, Confirm cmd) -> confirmDialog cmd
          Just (DialogCancel, Cancel) -> cancelDialog
          _ -> pure ()
      EvKey KEsc [] -> cancelDialog
      EvKey (KChar 'q') [] -> cancelDialog
      ev -> zoom (dialogL . _Just) $ D.handleDialogEvent ev

quit :: EventM n State ()
quit = focussedBranchesL %= L.listClear >> halt

focusBranches :: RemoteName -> EventM Name State ()
focusBranches target = do
  let isAlreadyFocussed = (target ==) <$> gets _focus
  unlessM isAlreadyFocussed $ do
    offsetDiff <- listOffsetDiff target
    modify (changeList . syncPosition offsetDiff)
 where
  changeList = focusL .~ target
  listIndex state = fromMaybe 0 $ state ^. currentListL . L.listSelectedL
  syncPosition diff state = (targetListL %~ L.listMoveTo (listIndex state - diff)) state
  (currentListL, targetListL) = case target of
    RLocal -> (remoteBranchesL, localBranchesL)
    RRemote -> (localBranchesL, remoteBranchesL)

listOffsetDiff :: RemoteName -> EventM Name State Int
listOffsetDiff target = do
  offLocal <- getOffset Local
  offRemote <- getOffset Remote
  pure
    $ if target == RLocal
      then offRemote - offLocal
      else offLocal - offRemote
 where
  getOffset name = maybe 0 (^. vpTop) <$> M.lookupViewport name

fetchBranches :: IO [Branch]
fetchBranches = do
  T.putStrLn "Fetching branches"
  output <- Git.fetch
  T.putStr output
  Git.listBranches

updateBranches :: [Branch] -> State -> State
updateBranches branches =
  syncBranchLists
    . (branchesL .~ branches)
    . (filterL .~ emptyFilter)

syncBranchLists :: State -> State
syncBranchLists state =
  state
    & localBranchesL
    .~ mkList Local local
    & remoteBranchesL
    .~ mkList Remote remote
    & focusL
    %~ toggleFocus (local, remote)
 where
  mkList name xs = L.list name (Vec.fromList xs) rowHeight
  filterText = T.toLower $ T.unwords $ E.getEditContents $ _filter state
  isBranchInFilter = T.isInfixOf filterText . Git.fullBranchName
  filteredBranches = filter isBranchInFilter (_branches state)
  (remote, local) = partition Git.isRemoteBranch filteredBranches

toggleFocus :: ([Branch], [Branch]) -> RemoteName -> RemoteName
toggleFocus ([], _ : _) RLocal = RRemote
toggleFocus (_ : _, []) RRemote = RLocal
toggleFocus _ x = x

selectedBranch :: State -> Maybe Branch
selectedBranch state =
  snd <$> L.listSelectedElement (state ^. focussedBranchesL)

createDialog :: GitCommand -> D.Dialog DialogOption Name
createDialog cmd = D.dialog (Just $ str title) (Just (DialogConfirm, choices)) 80
 where
  title = map toUpper $ show cmd
  btnText (x : xs) = toUpper x : xs
  btnText x = x
  choices =
    [ (btnText $ show cmd, DialogConfirm, Confirm cmd)
    , ("Cancel", DialogCancel, Cancel)
    ]

mapKey :: (Char -> Key) -> Event -> Event
mapKey f (EvKey (KChar k) []) = EvKey (f k) []
mapKey _ e = e

lowerKey :: Event -> Event
lowerKey = mapKey (KChar . toLower)

vimifiedKey :: Event -> Event
vimifiedKey = mapKey vimify . lowerKey
 where
  vimify 'h' = KLeft
  vimify 'j' = KRight
  vimify 'k' = KLeft
  vimify 'l' = KRight
  vimify k = KChar k

rowHeight :: Int
rowHeight = 1

-- Lens

focussedBranchesL :: Lens' State (L.List Name Branch)
focussedBranchesL =
  lens (\s -> s ^. branchLens s) (\s bs -> (branchLens s .~ bs) s)
 where
  branchLens s = case s ^. focusL of
    RLocal -> localBranchesL
    RRemote -> remoteBranchesL

localBranchesL :: Lens' State (L.List Name Branch)
localBranchesL = lens _localBranches (\s bs -> s{_localBranches = bs})

remoteBranchesL :: Lens' State (L.List Name Branch)
remoteBranchesL = lens _remoteBranches (\s bs -> s{_remoteBranches = bs})

focusL :: Lens' State RemoteName
focusL = lens _focus (\s f -> s{_focus = f})

filterL :: Lens' State (E.Editor Text Name)
filterL = lens _filter (\s f -> s{_filter = f})

branchesL :: Lens' State [Branch]
branchesL = lens _branches (\s f -> s{_branches = f})

isEditingFilterL :: Lens' State Bool
isEditingFilterL = lens _isEditingFilter (\s f -> s{_isEditingFilter = f})

dialogL :: Lens' State (Maybe (D.Dialog DialogOption Name))
dialogL = lens _dialog (\s v -> s{_dialog = v})

gitCommandL :: Lens' State GitCommand
gitCommandL = lens _gitCommand (\s v -> s{_gitCommand = v})
