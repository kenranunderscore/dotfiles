{
  description = "My system configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim-overlay.url = "github:nix-community/neovim-nightly-overlay";
    privateConfig.url =
      "git+ssh://git@github.com/kenranunderscore/private-config";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      overlays = [ inputs.emacs-overlay.overlay inputs.neovim-overlay.overlay ];
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit overlays system;
      };
      specialArgs = { inherit inputs; };
    in {
      nixosConfigurations =
        # FIXME(Johannes): create lib; improve username handling
        # (they're still repeated everywhere...). Maybe via
        # specialArgs? Also bundle home + system modules somehow (or
        # have a mechanism to detect corresponding ones; via
        # hostname).
        let
          mkNixosSystem = { hostname, username ? "kenran" }:
            let directory = ./hosts + "/${hostname}";
            in nixpkgs.lib.nixosSystem {
              inherit system pkgs specialArgs;
              modules = [
                (directory + /configuration.nix)
                home-manager.nixosModules.home-manager
                {
                  home-manager.users.${username} =
                    import (directory + /home.nix);
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = false;
                  home-manager.extraSpecialArgs = specialArgs;
                }
              ];
            };
        in {
          atuan = mkNixosSystem { hostname = "atuan"; };
          zangief = mkNixosSystem {
            hostname = "zangief";
            username = "johannes";
          };
          paln = mkNixosSystem { hostname = "paln"; };
        };
    };
}
