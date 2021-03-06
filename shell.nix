{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base-unicode-symbols, bytestring
      , containers, extra, hashable, optparse-applicative
      , optparse-generic, random-shuffle, stdenv, text, unordered-containers
      , freer, lens, MissingK
      , stack, intero, nix
      }:
      mkDerivation {
        pname = "gamesim";
        version = "0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base base-unicode-symbols bytestring containers extra hashable
          optparse-applicative optparse-generic random-shuffle text
          unordered-containers
	  freer lens MissingK
	  stack intero nix
        ];
        description = "Game simulator";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
