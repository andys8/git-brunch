module Lib
    ( gitBrunch
    ) where


import Brick

ui :: Widget ()
ui = str "Hello, world!"

gitBrunch :: IO ()
gitBrunch = simpleMain ui
