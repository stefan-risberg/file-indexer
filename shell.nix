{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation
      , base
      , text
      , filepath
      , containers
      , conduit
      , conduit-extra
      , conduit-combinators
      , convertible
      , bytestring
      , cryptohash
      , largeword

      , persistent
      , persistent-template
      , persistent-sqlite
      , esqueleto

      , lens
      , stdenv }:
      mkDerivation {
        pname = "test";
        version = "0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
            base
            text
            filepath
            containers
            conduit
            conduit-extra
            conduit-combinators
            convertible
            bytestring
            cryptohash
            largeword

            persistent
            persistent-template
            persistent-sqlite
            esqueleto

            lens
        ];
        license = stdenv.lib.licenses.bsd2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
