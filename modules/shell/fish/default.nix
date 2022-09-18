{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish.enable = lib.mkEnableOption "fish";

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      plugins = [{
        name = "autopair.fish";
        src = inputs."autopair.fish";
      }];
      shellAbbrs = import ../shell-aliases.nix { inherit pkgs; };
      interactiveShellInit = ''
        set fish_greeting
      '';
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
