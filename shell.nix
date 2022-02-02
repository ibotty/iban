let
  pins = import nix/pins.nix { };
in
(import ./. {
  inherit (pins) haskellNixSrc nixpkgsSrc;
  # until https://github.com/ndmitchell/ghcid/issues/350 is solved:
  system = "x86_64-darwin";
}).shell
