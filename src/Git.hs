module Git
  ( listBranches
  , checkout
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


toBranches :: String -> [Branch]
toBranches input = filter (not . isHead) $ toBranch <$> lines input


toBranch :: String -> Branch
toBranch line = toBranch' $ head $ words $ drop 2 line
 where
  isCurrent = "*" `isPrefixOf` line
  toBranch' name
    | isCurrent = BranchCurrent name
    | otherwise = case stripPrefix "remotes/" name of
      Just rest -> parseRemoteBranch rest
      Nothing   -> BranchLocal name


checkout :: Branch -> IO (Either String String)
checkout branch = toEither <$> execGitCheckout (branchName branch)
 where
  execGitCheckout name = readProcessWithExitCode "git" ["checkout", name] []
  toEither (ExitSuccess  , stdout, _     ) = Right $ dropWhile isSpace stdout
  toEither (ExitFailure _, _     , stderr) = Left $ dropWhile isSpace stderr


parseRemoteBranch :: String -> Branch
parseRemoteBranch str = BranchRemote remote name
  where (remote, _ : name) = span ('/' /=) str


--- Helper

branchName :: Branch -> String
branchName (BranchCurrent n ) = n
branchName (BranchLocal   n ) = n
branchName (BranchRemote _ n) = n


isHead :: Branch -> Bool
isHead = (== "HEAD") . branchName
