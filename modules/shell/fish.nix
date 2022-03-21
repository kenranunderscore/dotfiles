{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish.enable = lib.mkEnableOption "fish";

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      functions = {
        mc = {
          description = "Create directory and cd into it";
          body = ''
            command mkdir -p $argv
            if test $status = 0
              cd $argv[(count $argv)]
            end
          '';
        };
      };
      plugins = [
        {
          name = "fish-fastdir";
          src = inputs.fish-fastdir;
        }
        {
          name = "autopair.fish";
          src = inputs."autopair.fish";
        }
      ];
      shellAbbrs = import ./shell-aliases.nix;
    };
  };
}
