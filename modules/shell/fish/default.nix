{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish.enable = lib.mkEnableOption "fish";

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      plugins = [
        {
          name = "autopair.fish";
          src = inputs."autopair.fish";
        }
        {
          name = "z";
          src = inputs.z;
        }
      ];
      shellAbbrs = import ../shell-aliases.nix { inherit pkgs; };
      interactiveShellInit = ''
        set fish_greeting
      '';
      shellAliases = {
        ls = "exa";
        l = "exa -lbF --group-directories-first --icons";
        ll = "exa -lbGF --group-directories-first --icons";
        la = "exa -labF --group-directories-first --icons";
        lla = "exa -labGF --group-directories-first --icons";
      };
    };

    # Manage functions manually
    xdg.configFile = {
      "fish/functions" = {
        source = ./functions;
        recursive = true;
      };
    };
  };
}
