module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_git_brunch (version)

import GitBrunch qualified

data Mode = RunGitBrunch | ShowVersion

main :: IO ()
main = run =<< execParser opts
 where
  opts =
    info
      (versionParser <|> pure RunGitBrunch <**> helper)
      (header "git-brunch - A git command-line tool to work with branches")

run :: Mode -> IO ()
run ShowVersion = putStrLn $ showVersion version
run RunGitBrunch = GitBrunch.main

versionParser :: Parser Mode
versionParser =
  flag' ShowVersion (long "version" <> short 'v' <> help "Show version")
