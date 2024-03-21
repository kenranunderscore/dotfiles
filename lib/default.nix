{ lib, ... }:

{
  readDirNames = path:
    builtins.attrNames
    (lib.filterAttrs (_: type: type == "directory") (builtins.readDir path));

  mkNixosSystem = { hostname, system, inputs, pkgs }:
    let
      inherit (inputs) home-manager nixpkgs;
      dir = ../hosts + "/${hostname}";
      custom = (import (dir + /custom.nix)) // { inherit hostname; };
      username = custom.username;
      specialArgs = { inherit inputs custom; };
    in nixpkgs.lib.nixosSystem {
      inherit system pkgs specialArgs;
      modules = [
        (dir + /configuration.nix)
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            users.${username} = import (dir + /home.nix);
            useGlobalPkgs = true;
            useUserPackages = false;
            extraSpecialArgs = specialArgs;
          };
        }
      ];
    };

  mkHomeConfiguration = { username, hostname, system, inputs, pkgs }:
    let
      inherit (inputs) home-manager;
      dir = ../home-configurations + "/${username}";
      custom = import (dir + /custom.nix);
    in home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs custom; };
      modules = [ (dir + /home.nix) ];
    };
}
