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
    in {
      nixosConfigurations =
        # TODO(Johannes): create lib
        let
          mkNixosSystem = hostname:
            let
              dir = ./hosts + "/${hostname}";
              customConfig = import (dir + /customConfig.nix);
              username = customConfig.username;
              specialArgs = { inherit inputs customConfig; };
            in nixpkgs.lib.nixosSystem {
              inherit system pkgs specialArgs;
              modules = [
                (dir + /configuration.nix)
                home-manager.nixosModules.home-manager
                {
                  home-manager.users.${username} = import (dir + /home.nix);
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = false;
                  home-manager.extraSpecialArgs = specialArgs;
                }
              ];
            };
          machines = builtins.attrNames
            (pkgs.lib.filterAttrs (_: type: type == "directory")
              (builtins.readDir ./hosts));
        in builtins.foldl' (acc: host: acc // { ${host} = mkNixosSystem host; })
        { } machines;
    };
}
