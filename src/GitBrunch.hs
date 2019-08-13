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
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core                       ( hLimit
                                                          , str
                                                          , vBox
                                                          , vLimit
                                                          , withAttr
                                                          , (<+>)
                                                          , padLeftRight
                                                          )
import qualified Brick.Widgets.List            as L
import qualified Data.Vector                   as Vec
import           Git
import           Data.Maybe                    as Maybe

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
 where
  label = str "Branch " <+> cur <+> str " of " <+> total
  cur   = case l ^. L.listSelectedL of
    Nothing -> str "-"
    Just i  -> str (show (i + 1))
  total = str $ show $ Vec.length $ l ^. L.listElementsL
  box   = B.borderWithLabel label $ L.renderList listDrawElement True l
  ui    = C.vCenter $ padLeftRight 1 $ vBox
    [ C.hCenter box
    , str " "
    , C.hCenter $ str "Press +/- to add/remove list elements."
    , C.hCenter $ str "Press Esc/Q to exit."
    ]

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

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
          if sel then withAttr customAttr (str $ "<" <> s <> ">") else str s
  in  str "Branch " <+> selStr (show a)

initialState :: [Branch] -> L.List () Branch
initialState branches = L.list () (Vec.fromList branches) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap
  V.defAttr
  [ (L.listAttr        , V.white `on` V.black)
  , (L.listSelectedAttr, V.red `on` V.white)
  , (customAttr        , fg V.cyan)
  ]

theApp :: M.App (L.List () Branch) e ()
theApp = M.App { M.appDraw         = drawUI
               , M.appChooseCursor = M.showFirstCursor
               , M.appHandleEvent  = appEvent
               , M.appStartEvent   = return
               , M.appAttrMap      = const theMap
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


printResult :: Either String String -> IO ()
printResult (Left  e  ) = putStr e
printResult (Right msg) = putStr msg
