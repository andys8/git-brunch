{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, brick, extra, hpack, hspec, lib
      , microlens, microlens-mtl, mtl, optparse-applicative, process
      , text, vector, vty
      }:
      mkDerivation {
        pname = "git-brunch";
        version = "1.7.2.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base brick extra hspec microlens microlens-mtl mtl
          optparse-applicative process text vector vty
        ];
        testHaskellDepends = [
          base brick extra hspec microlens microlens-mtl mtl
          optparse-applicative process text vector vty
        ];
        prePatch = "hpack";
        homepage = "https://github.com/andys8/git-brunch#readme";
        description = "git checkout command-line tool";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
