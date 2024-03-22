{ lib, ... }:

{
  readDirNames = dir:
    builtins.map (path: dir + "/${path}")
    (builtins.attrNames (builtins.readDir dir));

  mkNixosSystem = { dir, system, inputs, pkgs }:
    let
      hostname = builtins.baseNameOf dir;
      custom = (import (dir + /custom.nix)) // { inherit hostname; };
      username = custom.username;
      specialArgs = { inherit inputs custom; };
    in {
      "${hostname}" = inputs.nixpkgs.lib.nixosSystem {
        inherit system pkgs specialArgs;
        modules = [
          (dir + /configuration.nix)
          inputs.home-manager.nixosModules.home-manager
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
    };

  mkHomeConfiguration = { dir, system, inputs, pkgs }:
    let
      userAtHost = builtins.baseNameOf dir;
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
