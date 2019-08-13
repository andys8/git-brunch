{-# LANGUAGE OverloadedStrings #-}
module GitBrunch where

import           Control.Monad                            ( void )
import           Data.Maybe                               ( fromMaybe )
import           Data.Monoid
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


drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l =
  [ C.vCenter $ padAll 1 $ vBox
      (C.hCenter branchBox : str " " : instructions)
  ]
 where
  branchBox =
    withBorderStyle BS.unicodeBold
      $ B.borderWithLabel (str "Branch")
      $ hLimit 100
      $ L.renderList listDrawElement True l
  instructions =
    [ drawInstruction "HJKL/arrows" "navigate"
    , drawInstruction "Enter"       "checkout"
    , drawInstruction "Esc/Q"       "exit"
    ]

drawInstruction :: String -> String -> Widget ()
drawInstruction keys action =
  C.hCenter
    $   str "Press "
    <+> withAttr "key" (str keys)
    <+> str " to "
    <+> withAttr "bold" (str action)
    <+> str "."

appEvent
  :: L.List () Branch
  -> T.BrickEvent () e
  -> T.EventM () (T.Next (L.List () Branch))
appEvent l (T.VtyEvent e) = case e of
  V.EvKey V.KEsc        [] -> M.halt $ L.listClear l
  V.EvKey (V.KChar 'q') [] -> M.halt $ L.listClear l
  V.EvKey V.KEnter      [] -> M.halt l
  ev -> M.continue =<< L.handleListEventVi L.handleListEvent ev l
appEvent l _ = M.continue l

listDrawElement :: Show a => Bool -> a -> Widget ()
listDrawElement selected a = C.hCenter $ str (show a)


attributeMap :: A.AttrMap
attributeMap = A.attrMap
  V.defAttr
  [ (L.listAttr        , fg V.white)
  , (L.listSelectedAttr, V.black `on` V.yellow)
  , (A.attrName "key", V.withStyle (V.brightYellow `on` V.black) V.bold)
  , (A.attrName "bold" , V.withStyle (fg V.white) V.bold)
  ]

initialState :: [Branch] -> L.List () Branch
initialState branches = L.list () (Vec.fromList branches) 1

theApp :: M.App (L.List () Branch) e ()
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
  let branch = snd <$> L.listSelectedElement finalState
  printResult
    =<< (case branch of
          Just branch -> Git.checkout branch
          Nothing     -> pure $ Left "No branch selected."
        )
 where
  printResult (Left  e  ) = putStr e
  printResult (Right msg) = putStr msg

