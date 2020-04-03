module Main where

import           Options.Applicative
import           Paths_git_brunch               ( version )
import           Data.Version                   ( showVersion )

import qualified GitBrunch


data Mode = RunGitBrunch | ShowVersion


main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (versionParser <|> pure RunGitBrunch <**> helper)
    (header "git-brunch - A git checkout and rebase command-line tool")


run :: Mode -> IO ()
run ShowVersion  = putStrLn $ showVersion version
run RunGitBrunch = GitBrunch.main


versionParser :: Parser Mode
versionParser =
  flag' ShowVersion (long "version" <> short 'v' <> help "Show version")

