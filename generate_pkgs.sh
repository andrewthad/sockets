CLEAN="cabal new-clean"
CONFIGURE="cabal new-configure"
PLAN="plan-to-nix --output . --plan-json dist-newstyle/cache/plan.json"

nix-shell -p haskellPackages.cabal-install haskellPackages.ghc --run \
  "$CLEAN && $CONFIGURE && $PLAN"
