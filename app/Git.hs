module Git
  ( Branch(..)
  , checkout
  , deleteBranch
  , fetch
  , fullBranchName
  , isCommonBranch
  , isRemoteBranch
  , listBranches
  , rebaseInteractive
  , merge
  , toBranches
  ) where

import           Data.Char                      ( isSpace )
import           Data.List
import           System.Exit
import           System.Process


data Branch = BranchLocal String
            | BranchCurrent String
            | BranchRemote String String
            deriving Eq

instance (Show Branch) where
  show (BranchLocal   n ) = n
  show (BranchCurrent n ) = n <> "*"
  show (BranchRemote o n) = o <> "/" <> n

fetch :: IO String
fetch = readGit ["fetch", "--all", "--prune"]

listBranches :: IO [Branch]
listBranches = toBranches <$> readGit
  [ "branch"
  , "--list"
  , "--all"
  , "--sort=-committerdate"
  , "--no-column"
  , "--no-color"
  ]

toBranches :: String -> [Branch]
toBranches input = toBranch <$> filter validBranch (lines input)
  where validBranch b = not $ isHead b || isDetachedHead b || isNoBranch b

toBranch :: String -> Branch
toBranch line = mkBranch $ words $ dropWhile isSpace line
 where
  mkBranch ("*" : name : _) = BranchCurrent name
  mkBranch (name       : _) = case stripPrefix "remotes/" name of
    Just rest -> parseRemoteBranch rest
    Nothing   -> BranchLocal name
  mkBranch [] = error "empty branch name"
  parseRemoteBranch str = BranchRemote remote name
    where (remote, _ : name) = span ('/' /=) str

checkout :: Branch -> IO ExitCode
checkout branch = spawnGit ["checkout", branchName branch]

rebaseInteractive :: Branch -> IO ExitCode
rebaseInteractive branch = do
  putStrLn $ "Rebase onto " <> fullBranchName branch
  spawnGit ["rebase", "--interactive", "--autostash", fullBranchName branch]

merge :: Branch -> IO ExitCode
merge branch = do
  putStrLn $ "Merge branch " <> fullBranchName branch
  spawnGit ["merge", fullBranchName branch]

deleteBranch :: Branch -> IO ExitCode
deleteBranch (BranchCurrent _ ) = error "Cannot delete current branch"
deleteBranch (BranchLocal   n ) = spawnGit ["branch", "-D", n]
deleteBranch (BranchRemote o n) = spawnGit ["push", o, "--delete", n]

spawnGit :: [String] -> IO ExitCode
spawnGit args = waitForProcess =<< spawnProcess "git" args

readGit :: [String] -> IO String
readGit args = readProcess "git" args []

isCommonBranch :: Branch -> Bool
isCommonBranch b =
  branchName b
    `elem` [ "master"
           , "main"
           , "dev"
           , "devel"
           , "develop"
           , "development"
           , "staging"
           , "trunk"
           ]


isRemoteBranch :: Branch -> Bool
isRemoteBranch (BranchRemote _ _) = True
isRemoteBranch _                  = False

--- Helper

branchName :: Branch -> String
branchName (BranchCurrent n ) = n
branchName (BranchLocal   n ) = n
branchName (BranchRemote _ n) = n

fullBranchName :: Branch -> String
fullBranchName (BranchCurrent n ) = n
fullBranchName (BranchLocal   n ) = n
fullBranchName (BranchRemote r n) = r <> "/" <> n

isHead :: String -> Bool
isHead = isInfixOf "HEAD"

isDetachedHead :: String -> Bool
isDetachedHead = isInfixOf "HEAD detached"

-- While rebasing git will show "no branch"
-- e.g. "* (no branch, rebasing branch-name)"
isNoBranch :: String -> Bool
isNoBranch = isInfixOf "(no branch,"
