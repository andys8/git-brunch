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
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vec
import Graphics.Vty hiding (update)
import Lens.Micro (Lens', lens, (%~), (&), (.~), (^.), _Just)
import Lens.Micro.Mtl ((%=), (.=))
import System.Exit

import Git (Branch (..))
import Git qualified
import Theme

data Name = Local | Remote | Filter deriving (Ord, Eq, Show)
data RemoteName = RLocal | RRemote deriving (Eq)
data GitCommand = GitRebase | GitMerge | GitCheckout | GitDeleteBranch deriving (Ord, Eq)
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

instance Show GitCommand where
  show GitCheckout = "checkout"
  show GitRebase = "rebase"
  show GitMerge = "merge"
  show GitDeleteBranch = "delete"

main :: IO ()
main = do
  branches <- Git.listBranches `catch` gitFailed
  state <- M.defaultMain app $ updateLists emptyState{_branches = branches}
  let execGit = gitFunction (_gitCommand state)
  exitCode <- maybe noBranchErr execGit (selectedBranch state)
  when (exitCode /= ExitSuccess) $
    die ("Failed to " ++ show (_gitCommand state) ++ ".")
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

emptyFilter :: E.Editor String Name
emptyFilter = E.editor Filter Nothing ""

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawApp
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appHandleEvent
    , M.appStartEvent = return ()
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
    maxWidth 100 $
      hBox
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
  editor = E.renderEditor (str . unlines) True (state ^. filterL)
  label = str " Filter: "

drawDialog :: State -> Widget n
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
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (drawTitle list) $
      L.renderList drawListElement hasFocus list
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

drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  withAttr attrKey (str keys)
    <+> str " to "
    <+> withAttr attrBold (str action)
    & C.hCenter

appHandleEvent :: BrickEvent Name e -> EventM Name State ()
appHandleEvent e
  | isQuitEvent e = quit
  | otherwise =
      gets _dialog >>= \case
        Nothing -> appHandleEventMain e
        Just d -> appHandleEventDialog d e
 where
  isQuitEvent (VtyEvent (EvKey (KChar 'c') [MCtrl])) = True
  isQuitEvent (VtyEvent (EvKey (KChar 'd') [MCtrl])) = True
  isQuitEvent _ = False

quit :: EventM n State ()
quit = focussedBranchesL %= L.listClear >> halt

appHandleEventDialog :: Dialog -> BrickEvent Name e -> EventM Name State ()
appHandleEventDialog dialog (VtyEvent e) = do
  let closeDialog = EndDialog Cancel
      dialogAction = case D.dialogSelection dialog of
        Just Cancel -> EndDialog Cancel
        Just confirm -> EndDialog confirm
        Nothing -> SetDialog dialog

      toState (SetDialog dlg) = dialogL .= Just dlg
      toState (EndDialog Confirm) = dialogL .= Nothing >> halt
      toState (EndDialog Cancel) = do
        dialogL .= Nothing
        gitCommandL .= GitCheckout
   in case vimKey $ lowerKey e of
        EvKey KEnter [] -> toState dialogAction
        EvKey KEsc [] -> toState closeDialog
        EvKey (KChar 'q') [] -> toState closeDialog
        ev -> zoom (dialogL . _Just) $ D.handleDialogEvent ev
appHandleEventDialog _ _ = pure ()

appHandleEventMain :: BrickEvent Name e -> EventM Name State ()
appHandleEventMain (VtyEvent e) =
  let
    confirm :: GitCommand -> EventM Name State ()
    confirm cmd = do
      gitCommandL .= cmd
      dialogL .= Just (createDialog cmd)

    confirmDelete :: Maybe Branch -> EventM Name State ()
    confirmDelete (Just (BranchCurrent _)) = pure ()
    confirmDelete (Just _) = confirm GitDeleteBranch
    confirmDelete Nothing = pure ()
    endWithCheckout = gitCommandL .= GitCheckout >> halt
    endWithRebase = gitCommandL .= GitRebase >> halt
    endWithMerge = gitCommandL .= GitMerge >> halt
    focusLocal = focusBranches RLocal
    focusRemote = focusBranches RRemote
    doFetch = do
      state <- get
      -- TODO: Refactor
      M.suspendAndResume (fetchBranches state)
    resetFilter = filterL .~ emptyFilter
    showFilter = isEditingFilterL .~ True
    hideFilter = isEditingFilterL .~ False
    startEditingFilter = modify (updateLists . resetFilter . showFilter)
    cancelEditingFilter = modify (hideFilter . resetFilter)
    stopEditingFilter = modify hideFilter

    handle event =
      gets _isEditingFilter >>= \case
        True -> handleEditingFilter event >> modify updateLists
        False -> handleDefault event

    handleDefault :: Event -> EventM Name State ()
    handleDefault = \case
      EvKey KEsc [] -> quit
      EvKey (KChar 'q') [] -> quit
      EvKey (KChar '/') [] -> startEditingFilter
      EvKey (KChar 'f') [MCtrl] -> startEditingFilter
      EvKey (KChar 'd') [] -> do
        state <- get
        confirmDelete (selectedBranch state)
      EvKey KEnter [] -> endWithCheckout
      EvKey (KChar 'c') [] -> endWithCheckout
      EvKey (KChar 'r') [] -> endWithRebase
      EvKey (KChar 'm') [] -> endWithMerge
      EvKey KLeft [] -> focusLocal
      EvKey (KChar 'h') [] -> focusLocal
      EvKey KRight [] -> focusRemote
      EvKey (KChar 'l') [] -> focusRemote
      EvKey (KChar 'f') [] -> doFetch
      _ -> navigate e

    handleEditingFilter :: Event -> EventM Name State ()
    handleEditingFilter = \case
      EvKey KEsc [] -> cancelEditingFilter
      EvKey KEnter [] -> stopEditingFilter
      EvKey KUp [] -> stopEditingFilter
      EvKey KDown [] -> stopEditingFilter
      _ -> handleFilter e
   in
    handle $ lowerKey e
appHandleEventMain _ = pure ()

navigate :: Event -> EventM Name State ()
navigate event =
  zoom focussedBranchesL $ L.handleListEventVi L.handleListEvent event

handleFilter :: Event -> EventM Name State ()
handleFilter event =
  zoom filterL $ E.handleEditorEvent (VtyEvent event)

focusBranches :: RemoteName -> EventM Name State ()
focusBranches target = do
  -- TODO: Refactor?
  focus <- gets _focus
  if focus == target
    then pure ()
    else do
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
  return $
    if target == RLocal then offRemote - offLocal else offLocal - offRemote
 where
  getOffset name = maybe 0 (^. vpTop) <$> M.lookupViewport name

fetchBranches :: State -> IO State
fetchBranches state = do
  putStrLn "Fetching branches"
  output <- Git.fetch
  putStr output
  branches <- Git.listBranches
  return $ updateLists state{_branches = branches, _filter = emptyFilter}

updateLists :: State -> State
updateLists state =
  -- TODO: Format?
  state
    & localBranchesL
      .~ mkList Local local
    & remoteBranchesL
      .~ mkList Remote remote
    & focusL
      %~ toggleFocus (local, remote)
 where
  mkList name xs = L.list name (Vec.fromList xs) rowHeight
  lower = map toLower
  filterString = lower $ unwords $ E.getEditContents $ _filter state
  isBranchInFilter = isInfixOf filterString . Git.fullBranchName
  filteredBranches = filter isBranchInFilter (_branches state)
  (remote, local) = partition Git.isRemoteBranch filteredBranches

toggleFocus :: ([Branch], [Branch]) -> RemoteName -> RemoteName
toggleFocus ([], _ : _) RLocal = RRemote
toggleFocus (_ : _, []) RRemote = RLocal
toggleFocus _ x = x

selectedBranch :: State -> Maybe Branch
selectedBranch state =
  snd <$> L.listSelectedElement (state ^. focussedBranchesL)

createDialog :: GitCommand -> Dialog
createDialog cmd = D.dialog (Just title) (Just (0, choices)) 80
 where
  choices = [(btnText $ show cmd, Confirm), ("Cancel", Cancel)]
  title = map toUpper $ show cmd
  btnText (x : xs) = toUpper x : xs
  btnText x = x

mapKey :: (Char -> Key) -> Event -> Event
mapKey f (EvKey (KChar k) []) = EvKey (f k) []
mapKey _ e = e

lowerKey :: Event -> Event
lowerKey = mapKey (KChar . toLower)

vimKey :: Event -> Event
vimKey = mapKey vimify
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
  let branchLens s = case s ^. focusL of
        RLocal -> localBranchesL
        RRemote -> remoteBranchesL
   in lens (\s -> s ^. branchLens s) (\s bs -> (branchLens s .~ bs) s)

localBranchesL :: Lens' State (L.List Name Branch)
localBranchesL = lens _localBranches (\s bs -> s{_localBranches = bs})

remoteBranchesL :: Lens' State (L.List Name Branch)
remoteBranchesL = lens _remoteBranches (\s bs -> s{_remoteBranches = bs})

focusL :: Lens' State RemoteName
focusL = lens _focus (\s f -> s{_focus = f})

filterL :: Lens' State (E.Editor String Name)
filterL = lens _filter (\s f -> s{_filter = f})

isEditingFilterL :: Lens' State Bool
isEditingFilterL = lens _isEditingFilter (\s f -> s{_isEditingFilter = f})

dialogL :: Lens' State (Maybe Dialog)
dialogL = lens _dialog (\s v -> s{_dialog = v})

gitCommandL :: Lens' State GitCommand
gitCommandL = lens _gitCommand (\s v -> s{_gitCommand = v})
