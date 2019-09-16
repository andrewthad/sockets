let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};

  pkgSet = with haskell; mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

  hsPkgs = pkgSet.config.hsPkgs // { _config = pkgSet.config; };

in rec {
  inherit hsPkgs;
}
