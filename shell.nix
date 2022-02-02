let
  pins = import nix/pins.nix { };
in
(import ./. { inherit (pins) haskellNixSrc nixpkgsSrc; }).shell
