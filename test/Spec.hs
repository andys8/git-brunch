import Git
import Test.Hspec

main :: IO ()
main = hspec $ describe "Git.toBranch" $ do
  it "returns a remote branch is starts with remote" $ do
    toBranches "remotes/origin/master" `shouldBe` [BranchRemote "origin" "master"]

  it "ignores leading spaces" $ do
    toBranches "  master" `shouldBe` [BranchLocal "master"]

  it "detects current branch by asterik" $ do
    toBranches "* master" `shouldBe` [BranchCurrent "master"]

  it "returns a local branch" $ do
    toBranches "master" `shouldBe` [BranchLocal "master"]

  it "returns a branch with head in name" $ do
    toBranches "updateHead" `shouldBe` [BranchLocal "updateHead"]

  it "ignores HEAD" $ do
    toBranches "HEAD" `shouldBe` []

  it "ignores empty" $ do
    toBranches "" `shouldBe` []

  it "ignores origin/HEAD" $ do
    toBranches "origin/HEAD" `shouldBe` []

  it "ignores detatched HEAD" $ do
    toBranches "* (HEAD detached at f01a202)" `shouldBe` []

  it "ignores 'no branch' during rebase" $ do
    toBranches "* (no branch, rebasing branch-name)" `shouldBe` []

  it "parses sample output" $ do
    toBranches sampleOutput
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
