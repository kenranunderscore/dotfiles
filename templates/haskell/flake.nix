{
  description = "A very basic Haskell project flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        "aarch64-linux"
      ];

      perSystem =
        { system, ... }:
        let
          ghc = "ghc96";
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (final: prev: {
                hspkgs = prev.haskell.packages.${ghc}.override (old: {
                  overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
                    hfinal: hprev: { myfoo = hfinal.callCabal2nix "foo" (final.lib.cleanSource ./.) { }; }
                  );
                });
              })
            ];
          };
        in
        {
          packages.default = pkgs.hspkgs.myfoo;

          devShells.default = pkgs.hspkgs.shellFor {
            packages = p: [ p.myfoo ];
            nativeBuildInputs = [
              pkgs.hspkgs.cabal-install
              pkgs.hspkgs.haskell-language-server
            ];
          };
        };
    };
}
