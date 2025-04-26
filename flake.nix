{
  description =
    "A library to validate and generate (for now only German) IBANs.";
  inputs.haskellNix.url =
    "github:input-output-hk/haskell.nix/cafa5223a5411fa7545f21d76e9b8743f4d00c29";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2411";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "aarch64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            ibanProject = final.haskell-nix.project' {
              src = final.haskell-nix.haskellLib.cleanGit { src = ./.; };
              compiler-nix-name = "ghc98";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = { };
                hlint = { };
                ghcid = { };
                haskell-language-server = { };
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [ nixpkgs-fmt ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.ibanProject.flake { };
      in flake // {
        # devShells.default = pkgs.ibanProject."iban".shellFor { };
      });
}
