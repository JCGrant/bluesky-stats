{
  description = "bluesky-stats";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              bluesky-stats = hfinal.callCabal2nix "bluesky-stats" ./. { };
            };
        };
        bluesky-stats = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.bluesky-stats;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = bluesky-stats-shell;
            bluesky-stats-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.bluesky-stats ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = bluesky-stats;
            bluesky-stats = pkgs.bluesky-stats;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
