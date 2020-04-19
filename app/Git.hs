module Git
  ( listBranches
  , checkout
  , fetch
  , rebaseInteractive
  , toBranches
  , deleteBranch
  , Branch(..)
  )
where

import           System.Process
import           Data.List
import           Data.Char                                ( isSpace )
import           System.Exit

data Branch = BranchLocal String
            | BranchCurrent String
            | BranchRemote String String
            deriving Eq


instance (Show Branch) where
  show (BranchLocal   n ) = n
  show (BranchCurrent n ) = n <> "*"
  show (BranchRemote o n) = o <> "/" <> n


listBranches :: IO [Branch]
listBranches = toBranches <$> execGitBranch
 where
  execGitBranch = readProcess
    "git"
    [ "branch"
    , "--list"
    , "--all"
    , "--sort=-committerdate"
    , "--no-column"
    , "--no-color"
    ]
    []

fetch :: IO String
fetch = readProcess "git" ["fetch", "--all", "--prune"] []

toBranches :: String -> [Branch]
toBranches input = toBranch <$> filter (not . isHead) (lines input)


toBranch :: String -> Branch
toBranch line = toBranch' $ words $ dropWhile isSpace line
 where
  toBranch' ("*" : name : _) = BranchCurrent name
  toBranch' (name       : _) = case stripPrefix "remotes/" name of
    Just rest -> parseRemoteBranch rest
    Nothing   -> BranchLocal name
  toBranch' [] = error "empty branch name"


checkout :: Branch -> IO ExitCode
checkout branch = spawnGit ["checkout", branchName branch]


rebaseInteractive :: Branch -> IO ExitCode
rebaseInteractive branch = do
  putStrLn $ "Rebase onto " <> fullBranchName branch
  spawnGit ["rebase", "--interactive", "--autostash", fullBranchName branch]

deleteBranch :: Branch -> IO ExitCode
deleteBranch (BranchCurrent _ ) = error "Cannot delete current branch"
deleteBranch (BranchLocal   n ) = spawnGit ["branch", "-D", n]
deleteBranch (BranchRemote o n) = spawnGit ["push", o, "--delete", n]

spawnGit :: [String] -> IO ExitCode
spawnGit args = waitForProcess =<< spawnProcess "git" args


parseRemoteBranch :: String -> Branch
parseRemoteBranch str = BranchRemote remote name
  where (remote, _ : name) = span ('/' /=) str


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
