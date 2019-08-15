{-# LANGUAGE OverloadedStrings #-}
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
                                                          , withAttr
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
data State = State { localBranches :: L.List Name Branch, remoteBranches :: L.List Name Branch }

drawUI :: State -> [Widget Name]
drawUI state =
  [ C.vCenter
      $ padAll 1
      $ vBox
          [ hBox [C.hCenter branchBox1, C.hCenter branchBox2]
          , str " "
          , instructions
          ]
  ]
 where
  branchBox1   = drawBranchList $ localBranches state
  branchBox2   = drawBranchList $ remoteBranches state
  instructions = vBox
    [ drawInstruction "HJKL/arrows" "navigate"
    , drawInstruction "Enter"       "checkout"
    , drawInstruction "Esc/Q"       "exit"
    ]

drawBranchList :: Show a => L.List Name a -> Widget Name
drawBranchList list =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str (title (L.listName list)))
    $ hLimit 100
    $ L.renderList listDrawElement hasFocus list
 where
  hasFocus = L.listName list == Local
  title Local  = map toUpper "local"
  title Remote = map toUpper "remote"

listDrawElement :: Show a => Bool -> a -> Widget Name
listDrawElement selected a = C.hCenter $ str (show a)

drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  C.hCenter
    $   str "Press "
    <+> withAttr "key" (str keys)
    <+> str " to "
    <+> withAttr "bold" (str action)
    <+> str "."

appEvent :: State -> T.BrickEvent Name e -> T.EventM Name (T.Next State)
appEvent state (T.VtyEvent e) = case e of
  V.EvKey V.KEsc [] ->
    M.halt $ state { localBranches = L.listClear $ localBranches state }
  V.EvKey (V.KChar 'q') [] ->
    M.halt $ state { localBranches = L.listClear $ localBranches state }

  V.EvKey V.KEnter [] -> M.halt state
  ev ->
    (M.continue . (\l -> state { localBranches = l }))
      =<< L.handleListEventVi L.handleListEvent ev (localBranches state)
appEvent state _ = M.continue state




attributeMap :: A.AttrMap
attributeMap = A.attrMap
  V.defAttr
  [ (L.listAttr        , fg V.white)
  , (L.listSelectedAttr, V.black `on` V.yellow)
  , (A.attrName "key", V.withStyle (V.brightYellow `on` V.black) V.bold)
  , (A.attrName "bold" , V.withStyle (fg V.white) V.bold)
  ]

initialState :: [Branch] -> State
initialState branches = State
  { localBranches  = L.list Local (Vec.fromList local) 1
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

main :: IO ()
main = do
  branches   <- Git.listBranches
  finalState <- M.defaultMain theApp (initialState branches)
  let branch = snd <$> L.listSelectedElement (localBranches finalState)
  printResult
    =<< (case branch of
          Just branch -> Git.checkout branch
          Nothing     -> pure $ Left "No branch selected."
        )
 where
  printResult (Left  e  ) = putStr e
  printResult (Right msg) = putStr msg

