module Git
  ( branch
  )
where

import           System.Process
import           Data.List
import           Data.Char                                ( isSpace )

data Branch = Branch String | CurrentBranch String deriving (Show)

branch = do
  input <- readProcess
    "git"
    ["branch", "--all", "--sort=-committerdate", "--color=never"]
    ""
  return $ mkBranches input


mkBranches :: String -> [Branch]
mkBranches input = map mkBranch $ lines input

mkBranch :: String -> Branch
mkBranch input = if isCurrent then CurrentBranch name else Branch name
 where
  isCurrent = "*" `isPrefixOf` input
  name      = head $ words $ drop 2 input

