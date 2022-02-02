{ nixpkgsSrc ? import <nixpkgs>
, haskellNixSrc ? import (
    builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz"
  )
, system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
}:
let
  lib = (nixpkgsSrc { }).lib;
  haskellNix = haskellNixSrc { };
  pkgsArgs = lib.recursiveUpdate haskellNix.nixpkgsArgs {
    inherit system crossSystem;
    config = lib.recursiveUpdate haskellNix.config config;
  };
  pkgs = nixpkgsSrc pkgsArgs;
  project = pkgs.haskell-nix.cabalProject' {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "iban-src";
      src = ./.;
    };
    compiler-nix-name = "ghc8107";
  };
in
{
  shell = project.shellFor
    {
      withHoogle = true;
      tools = {
        cabal = "latest";
        hlint = "latest";
        ghcid = "latest";
      };
      exactDeps = true;
    };
}
