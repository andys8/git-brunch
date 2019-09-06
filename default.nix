# cabal2nix --shell . > default.nix
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, brick, hpack, microlens, process
      , stdenv, vector, vty
      }:
      mkDerivation {
        pname = "git-brunch";
        version = "1.0.6.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base brick microlens process vector vty
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base brick microlens process vector vty
        ];
        testHaskellDepends = [ base brick microlens process vector vty ];
        prePatch = "hpack";
        homepage = "https://github.com/andys8/git-brunch#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
