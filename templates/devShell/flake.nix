{
  description = "Development environment for this project";

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
          pkgs = import nixpkgs { inherit system; };
        in
        {
          devShells.default =
            let
              libraries = [ ];
            in
            pkgs.mkShell {
              buildInputs = libraries;
              LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libraries;
            };
        };
    };
}
