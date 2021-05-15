{-# LANGUAGE LambdaCase #-}
module GitBrunch
  ( main
  ) where

import           Brick.Main                     ( continue
                                                , halt
                                                , suspendAndResume
                                                )
import qualified Brick.Main                    as M
import           Brick.Themes                   ( themeToAttrMap )
import           Brick.Types
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core
import qualified Brick.Widgets.Dialog          as D
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.List            as L
import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Vector                   as Vec
import           Graphics.Vty            hiding ( update )
import           Lens.Micro                     ( (%~)
                                                , (&)
                                                , (.~)
                                                , Lens'
                                                , (^.)
                                                , lens
                                                )
import           System.Exit

import           Git                            ( Branch(..) )
import qualified Git
import           Theme


data Name         = Local | Remote | Filter deriving (Ord, Eq, Show)
data RemoteName   = RLocal | RRemote deriving (Eq)
data GitCommand   = GitRebase | GitMerge | GitCheckout | GitDeleteBranch deriving (Ord, Eq)
data DialogResult = SetDialog Dialog | EndDialog DialogOption
data DialogOption = Cancel | Confirm
type Dialog = D.Dialog DialogOption

data State = State
  { _focus           :: RemoteName
  , _gitCommand      :: GitCommand
  , _branches        :: [Branch]
  , _localBranches   :: L.List Name Branch
  , _remoteBranches  :: L.List Name Branch
  , _dialog          :: Maybe Dialog
  , _filter          :: E.Editor String Name
  , _isEditingFilter :: Bool
  }


instance Show GitCommand where
  show GitCheckout     = "checkout"
  show GitRebase       = "rebase"
  show GitMerge        = "merge"
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
    GitMerge        -> Git.merge
    GitDeleteBranch -> Git.deleteBranch

emptyState :: State
emptyState =
  let mkList focus = L.list focus Vec.empty rowHeight
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
            , M.appHandleEvent  = appHandleWithQuit
            , M.appStartEvent   = return
            , M.appAttrMap      = const $ themeToAttrMap theme
            }

appDraw :: State -> [Widget Name]
appDraw state =
  drawDialog state
    : [ C.vCenter $ padAll 1 $ maxWidth 200 $ vBox
          [branchLists, filterEdit, padding, instructions]
      ]
 where
  padding = str " "
  maxWidth w = C.hCenter . hLimit w
  toBranchList r lens' =
    let isActive = state ^. focusL == r && not (_isEditingFilter state)
    in  state ^. lens' & drawBranchList isActive
  filterEdit  = if _isEditingFilter state then drawFilter state else emptyWidget
  branchLists = hBox
    [ C.hCenter $ toBranchList RLocal localBranchesL
    , str " "
    , C.hCenter $ toBranchList RRemote remoteBranchesL
    ]
  instructions = maxWidth 100 $ hBox
    [ drawInstruction "Enter" "checkout"
    , drawInstruction "/"     "filter"
    , drawInstruction "F"     "fetch"
    , drawInstruction "R"     "rebase"
    , drawInstruction "M"     "merge"
    , drawInstruction "D"     "delete"
    ]

drawFilter :: State -> Widget Name
drawFilter state =
  withBorderStyle BS.unicodeBold $ B.border $ vLimit 1 $ label <+> editor
 where
  editor = E.renderEditor (str . unlines) True (state ^. filterL)
  label  = str " Filter: "

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
drawListElement isListFocussed branch =
  maxPadding $ highlight branch $ str $ " " <> show branch
 where
  maxPadding = if isListFocussed then padRight Max else id
  highlight (BranchCurrent _)        = withAttr attrBranchCurrent
  highlight b | Git.isCommonBranch b = withAttr attrBranchCommon
  highlight _                        = id

drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  withAttr attrKey (str keys)
    <+> str " to "
    <+> withAttr attrBold (str action)
    &   C.hCenter

appHandleWithQuit :: State -> BrickEvent Name e -> EventM Name (Next State)
appHandleWithQuit state e = if isQuitEvent e
  then quit state
  else appHandleEvent state e
 where
  isQuitEvent (VtyEvent (EvKey (KChar 'c') [MCtrl])) = True
  isQuitEvent (VtyEvent (EvKey (KChar 'd') [MCtrl])) = True
  isQuitEvent _ = False

quit :: State -> EventM Name (Next State)
quit state = halt $ focussedBranchesL %~ L.listClear $ state

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
  let closeDialog  = pure $ EndDialog Cancel
      dialogAction = pure $ case D.dialogSelection dialog of
        Just Cancel  -> EndDialog Cancel
        Just confirm -> EndDialog confirm
        Nothing      -> SetDialog dialog
  in  case vimKey $ lowerKey e of
        EvKey KEnter      [] -> dialogAction
        EvKey KEsc        [] -> closeDialog
        EvKey (KChar 'q') [] -> closeDialog
        ev                   -> SetDialog <$> D.handleDialogEvent ev dialog
appHandleEventDialog dialog _ = pure $ SetDialog dialog

appHandleEventMain :: State -> BrickEvent Name e -> EventM Name (Next State)
appHandleEventMain state (VtyEvent e) =
  let
    confirm c = state { _gitCommand = c, _dialog = Just $ createDialog c }
    confirmDelete (Just (BranchCurrent _)) = continue state
    confirmDelete (Just _                ) = continue $ confirm GitDeleteBranch
    confirmDelete Nothing                  = continue state
    endWithCheckout = halt $ state { _gitCommand = GitCheckout }
    endWithRebase   = halt $ state { _gitCommand = GitRebase }
    endWithMerge    = halt $ state { _gitCommand = GitMerge }
    focusLocal      = focusBranches RLocal state
    focusRemote     = focusBranches RRemote state
    doFetch         = suspendAndResume (fetchBranches state)
    resetFilter     = filterL .~ emptyFilter
    showFilter      = isEditingFilterL .~ True
    hideFilter      = isEditingFilterL .~ False
    startEditingFilter =
      continue $ updateLists $ resetFilter $ showFilter state
    cancelEditingFilter = continue $ hideFilter $ resetFilter state
    stopEditingFilter   = continue $ hideFilter state
    handle              = if _isEditingFilter state
      then fmap (updateLists <$>) . handleEditingFilter
      else handleDefault
    handleDefault = \case
      EvKey KEsc        []      -> quit state
      EvKey (KChar 'q') []      -> quit state
      EvKey (KChar '/') []      -> startEditingFilter
      EvKey (KChar 'f') [MCtrl] -> startEditingFilter
      EvKey (KChar 'd') []      -> confirmDelete (selectedBranch state)
      EvKey KEnter      []      -> endWithCheckout
      EvKey (KChar 'c') []      -> endWithCheckout
      EvKey (KChar 'r') []      -> endWithRebase
      EvKey (KChar 'm') []      -> endWithMerge
      EvKey KLeft       []      -> focusLocal
      EvKey (KChar 'h') []      -> focusLocal
      EvKey KRight      []      -> focusRemote
      EvKey (KChar 'l') []      -> focusRemote
      EvKey (KChar 'f') []      -> doFetch
      _                         -> navigate state e
    handleEditingFilter = \case
      EvKey KEsc   [] -> cancelEditingFilter
      EvKey KEnter [] -> stopEditingFilter
      EvKey KUp    [] -> stopEditingFilter
      EvKey KDown  [] -> stopEditingFilter
      _               -> handleFilter state e
  in
    handle $ lowerKey e

appHandleEventMain state _ = continue state


navigate :: State -> Event -> EventM Name (Next State)
navigate state event =
  continue =<< handleEventLensed state focussedBranchesL update event
  where update = L.handleListEventVi L.handleListEvent

handleFilter :: State -> Event -> EventM Name (Next State)
handleFilter state event =
  continue =<< handleEventLensed state filterL E.handleEditorEvent event

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
  output <- Git.fetch
  putStr output
  branches <- Git.listBranches
  return $ updateLists state { _branches = branches, _filter = emptyFilter }

updateLists :: State -> State
updateLists state =
  state
    &  localBranchesL
    .~ mkList Local local
    &  remoteBranchesL
    .~ mkList Remote remote
    &  focusL
    %~ toggleFocus (local, remote)
 where
  mkList name xs = L.list name (Vec.fromList xs) rowHeight
  lower            = map toLower
  filterString     = lower $ unwords $ E.getEditContents $ _filter state
  isBranchInFilter = isInfixOf filterString . Git.fullBranchName
  filteredBranches = filter isBranchInFilter (_branches state)
  (remote, local)  = partition Git.isRemoteBranch filteredBranches

toggleFocus :: ([Branch], [Branch]) -> RemoteName -> RemoteName
toggleFocus ([]   , _ : _) RLocal  = RRemote
toggleFocus (_ : _, []   ) RRemote = RLocal
toggleFocus _              x       = x

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

rowHeight :: Int
rowHeight = 1

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

isEditingFilterL :: Lens' State Bool
isEditingFilterL = lens _isEditingFilter (\s f -> s { _isEditingFilter = f })
