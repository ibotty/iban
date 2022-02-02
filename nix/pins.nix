{ haskellNixRev ? "fd13635f705956a85b8f59dd90b4bdc6a1319d44"
}:
rec {
  haskellNixSrc = import
    (
      builtins.fetchTarball
        "https://github.com/input-output-hk/haskell.nix/archive/${haskellNixRev}.tar.gz"
    );
  nixpkgsSrc = import (haskellNixSrc { }).sources.nixpkgs-2111;
}
