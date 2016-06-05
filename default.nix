{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, hint, HTTP, hylogen
      , servant-client, servant-server, stdenv, text, transformers, wai
      , warp
      }:
      mkDerivation {
        pname = "try-hylogen";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base hint HTTP hylogen servant-client servant-server text
          transformers wai warp
        ];
        homepage = "hylogen.com";
        description = "try hylogen in your browser";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
