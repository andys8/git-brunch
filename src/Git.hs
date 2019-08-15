module Git
  ( listBranches
  , checkout
  , Branch(..)
  )
where

import           System.Process
import           Data.List
import           Data.Char                                ( isSpace )
import           Data.Either
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
    ["branch", "--all", "--sort=-committerdate", "--color=never"]
    []


toBranches :: String -> [Branch]
toBranches input = toBranch <$> lines input

toBranch :: String -> Branch
toBranch line = toBranch $ head $ words $ drop 2 line
 where
  isCurrent = "*" `isPrefixOf` line
  toBranch name
    | isCurrent = BranchCurrent name
    | otherwise = case stripPrefix "remotes/" name of
      Just rest -> parseRemoteBranch rest
      Nothing   -> BranchLocal name

checkout :: Branch -> IO (Either String String)
checkout branch =
  let
    execGitCheckout name = readProcessWithExitCode "git" ["checkout", name] []
    toEither (ExitSuccess  , stdout, stderr) = Right $ dropWhile isSpace stdout
    toEither (ExitFailure _, stdout, stderr) = Left $ dropWhile isSpace stderr
  in
    case branch of
      BranchCurrent name  -> pure $ Left $ "Already on branch " ++ name
      BranchLocal   name  -> toEither <$> execGitCheckout name
      BranchRemote _ name -> toEither <$> execGitCheckout name


parseRemoteBranch :: String -> Branch
parseRemoteBranch str = BranchRemote remote branchName
  where (remote, _ : branchName) = span ('/' /=) str

