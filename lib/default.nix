{ lib, ... }:

{
  readDirNames = path: builtins.attrNames (builtins.readDir path);

  mkNixosSystem = { hostname, system, inputs, pkgs }:
    let
      inherit (inputs) home-manager nixpkgs;
      dir = ../nixos + "/${hostname}";
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

  mkHomeConfiguration = { userAtHost, system, inputs, pkgs }:
    let
      dir = ../users + "/${userAtHost}";
      parts = lib.splitString "@" userAtHost;
      username = builtins.head parts;
      hostname = builtins.elemAt parts 1;
      custom = import (dir + /custom.nix) // { inherit username hostname; };
    in {
      "${username}" = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = { inherit inputs custom; };
        modules = [ (dir + /home.nix) ];
      };
    };
}
