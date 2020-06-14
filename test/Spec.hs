import           Test.Hspec
import           Git

main :: IO ()
main = hspec $ describe "Git.toBranch" $ do

  it "returns a remote branch is starts with remote"
    $          toBranches "remotes/origin/master"
    `shouldBe` [BranchRemote "origin" "master"]

  it "ignores leading spaces"
    $          toBranches "  master"
    `shouldBe` [BranchLocal "master"]

  it "detects current branch by asterik"
    $          toBranches "* master"
    `shouldBe` [BranchCurrent "master"]

  it "returns a local branch"
    $          toBranches "master"
    `shouldBe` [BranchLocal "master"]

  it "returns a branch with head in name"
    $          toBranches "updateHead"
    `shouldBe` [BranchLocal "updateHead"]

  it "ignores HEAD" $ toBranches "HEAD" `shouldBe` []

  it "ignores origin/HEAD" $ toBranches "origin/HEAD" `shouldBe` []

  it "ignores detatched HEAD"
    $          toBranches "* (HEAD detached at f01a202)"
    `shouldBe` []

  it "parses sample output"
    $          toBranches sampleOutput
    `shouldBe` [ BranchLocal "experimental/failing-debug-log-demo"
               , BranchLocal "gh-pages"
               , BranchLocal "master"
               , BranchLocal "wip/delete-as-action"
               , BranchRemote "origin" "experimental/failing-debug-log-demo"
               , BranchRemote "origin" "gh-pages"
               , BranchRemote "origin" "master"
               ]

sampleOutput :: String
sampleOutput =
  "* (HEAD detached at f01a202)\n"
    ++ "  experimental/failing-debug-log-demo\n"
    ++ "  gh-pages\n"
    ++ "  master\n"
    ++ "  wip/delete-as-action\n"
    ++ "  remotes/origin/HEAD -> origin/master\n"
    ++ "  remotes/origin/experimental/failing-debug-log-demo\n"
    ++ "  remotes/origin/gh-pages\n"
    ++ "  remotes/origin/master"
