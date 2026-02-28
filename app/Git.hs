module Git (
  Branch (..),
  checkout,
  deleteBranch,
  fetch,
  fullBranchName,
  isCommonBranch,
  isRemoteBranch,
  listBranches,
  getWorktreePath,
  rebaseInteractive,
  merge,
  toBranches,
) where

import Data.Char (isSpace)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit
import System.Process

data Branch
  = BranchLocal Text
  | BranchCurrent Text
  | BranchWorktree Text
  | BranchRemote Text Text
  deriving (Eq)

instance Show Branch where
  show (BranchLocal n) = T.unpack n
  show (BranchCurrent n) = T.unpack $ n <> "*"
  show (BranchWorktree n) = T.unpack $ n <> "+"
  show (BranchRemote o n) = T.unpack $ o <> "/" <> n

fetch :: IO Text
fetch = readGit ["fetch", "--all", "--prune"]

listBranches :: IO [Branch]
listBranches =
  toBranches
    <$> readGit
      [ "branch"
      , "--list"
      , "--all"
      , "--sort=-committerdate"
      , "--no-column"
      , "--no-color"
      ]

toBranches :: Text -> [Branch]
toBranches input = toBranch <$> filter validBranch (T.lines input)
 where
  validBranch b = not $ isHead b || isDetachedHead b || isNoBranch b

toBranch :: Text -> Branch
toBranch line = mkBranch $ T.words $ T.dropWhile isSpace line
 where
  mkBranch ("*" : name : _) = BranchCurrent name
  mkBranch ("+" : name : _) = BranchWorktree name
  mkBranch (name : _) = case T.stripPrefix "remotes/" name of
    Just rest -> parseRemoteBranch rest
    Nothing -> BranchLocal name
  mkBranch [] = error "empty branch name"
  parseRemoteBranch str = BranchRemote remote name
   where
    (remote, rest) = T.span ('/' /=) str
    name = T.drop 1 rest

checkout :: Branch -> IO ExitCode
checkout (BranchWorktree n) = do
  mPath <- getWorktreePath n
  case mPath of
    Just path -> spawnShell path
    Nothing -> spawnGit ["checkout", n]
checkout branch = spawnGit ["checkout", branchName branch]

spawnShell :: FilePath -> IO ExitCode
spawnShell path = do
  setCurrentDirectory path
  userShell <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
  T.putStrLn $ "Switching to worktree at " <> T.pack path
  T.putStrLn "Spawning subshell. Press Ctrl+D or type 'exit' to return."
  waitForProcess =<< spawnProcess userShell []

rebaseInteractive :: Branch -> IO ExitCode
rebaseInteractive branch = do
  T.putStrLn $ "Rebase onto " <> fullBranchName branch
  spawnGit ["rebase", "--interactive", "--autostash", fullBranchName branch]

merge :: Branch -> IO ExitCode
merge branch = do
  T.putStrLn $ "Merge branch " <> fullBranchName branch
  spawnGit ["merge", fullBranchName branch]

deleteBranch :: Branch -> IO ExitCode
deleteBranch (BranchCurrent _) = error "Cannot delete current branch"
deleteBranch (BranchWorktree _) = error "Cannot delete branch checked out in another worktree"
deleteBranch (BranchLocal n) = spawnGit ["branch", "-D", n]
deleteBranch (BranchRemote o n) = spawnGit ["push", o, "--delete", n]

spawnGit :: [Text] -> IO ExitCode
spawnGit args = waitForProcess =<< spawnProcess "git" (T.unpack <$> args)

readGit :: [Text] -> IO Text
readGit args = T.pack <$> readProcess "git" (T.unpack <$> args) []

getWorktreePath :: Text -> IO (Maybe FilePath)
getWorktreePath branch = do
  output <- readGit ["worktree", "list", "--porcelain"]
  let worktrees = parseWorktrees (T.lines output)
  pure $ Map.lookup branch worktrees

parseWorktrees :: [Text] -> Map Text FilePath
parseWorktrees lines' = go lines' Map.empty Nothing
 where
  go [] m _ = m
  go (l : ls) m currentPath
    | "worktree " `T.isPrefixOf` l = go ls m (Just $ T.unpack $ T.drop 9 l)
    | "branch " `T.isPrefixOf` l =
        let branch = fromMaybe (T.drop 13 l) (T.stripPrefix "refs/heads/" (T.drop 7 l))
         in case currentPath of
              Just p -> go ls (Map.insert branch p m) Nothing
              Nothing -> go ls m Nothing
    | otherwise = go ls m currentPath

isCommonBranch :: Branch -> Bool
isCommonBranch b = branchName b `elem` commonBranchNames
 where
  commonBranchNames =
    [ "master"
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
isRemoteBranch _ = False

--- Helper

branchName :: Branch -> Text
branchName (BranchCurrent n) = n
branchName (BranchWorktree n) = n
branchName (BranchLocal n) = n
branchName (BranchRemote _ n) = n

fullBranchName :: Branch -> Text
fullBranchName (BranchCurrent n) = n
fullBranchName (BranchWorktree n) = n
fullBranchName (BranchLocal n) = n
fullBranchName (BranchRemote r n) = r <> "/" <> n

isHead :: Text -> Bool
isHead = T.isInfixOf "HEAD"

isDetachedHead :: Text -> Bool
isDetachedHead = T.isInfixOf "HEAD detached"

-- While rebasing git will show "no branch"
-- e.g. "* (no branch, rebasing branch-name)"
isNoBranch :: Text -> Bool
isNoBranch = T.isInfixOf "(no branch,"
