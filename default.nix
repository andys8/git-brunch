{ nixpkgs ? import <nixpkgs> { }, compiler ? "default", doBenchmark ? false }:
let
  inherit (nixpkgs) pkgs;

  f =
    { mkDerivation
    , base
    , brick
    , hpack
    , hspec
    , microlens
    , optparse-applicative
    , process
    , stdenv
    , vector
    , vty
    }:
    mkDerivation {
      pname = "git-brunch";
      version = "1.5.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      libraryToolDepends = [ hpack ];
      executableHaskellDepends = [
        base
        brick
        hspec
        microlens
        optparse-applicative
        process
        vector
        vty
      ];
      testHaskellDepends = [
        base
        brick
        hspec
        microlens
        optparse-applicative
        process
        vector
        vty
      ];
      prePatch = "hpack";
      homepage = "https://github.com/andys8/git-brunch#readme";
      description = "git checkout command-line tool";
      license = stdenv.lib.licenses.bsd3;
    };

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { });

in
if pkgs.lib.inNixShell then drv.env else drv
