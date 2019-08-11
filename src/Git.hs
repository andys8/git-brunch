module Git
  ( branch
  )
where

import           System.Process
import           Data.List
import           Data.Char                                ( isSpace )

data Branch = BranchLocal String
            | BranchCurrent String
            | BranchRemote String
              deriving (Show)

branch = mkBranches <$> execGitBranch
 where
  execGitBranch = readProcess
    "git"
    ["branch", "--all", "--sort=-committerdate", "--color=never"]
    []


mkBranches :: String -> [Branch]
mkBranches input = map mkBranch $ lines input

mkBranch :: String -> Branch
mkBranch line = toBranch $ head $ words $ drop 2 line
 where
  isCurrent = "*" `isPrefixOf` line
  toBranch name
    | isCurrent = BranchCurrent name
    | otherwise = case stripPrefix "remotes/" name of
      Just rest -> BranchRemote rest
      Nothing   -> BranchLocal name
