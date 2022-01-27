{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish.enable = lib.mkEnableOption "fish";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.starship ];

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
          src = pkgs.fetchFromGitHub {
            owner = "danhper";
            repo = "fish-fastdir";
            rev = "dddc6c13b4afe271dd91ec004fdd199d3bbb1602";
            sha256 = "sha256-iu7zNO7yKVK2bhIIlj4UKHHqDaGe4q2tIdNgifxPev4=";
          };
        }
        {
          name = "autopair.fish";
          src = pkgs.fetchFromGitHub {
            owner = "jorgebucaran";
            repo = "autopair.fish";
            rev = "1222311994a0730e53d8e922a759eeda815fcb62";
            sha256 = "sha256-l6WJ2kjDO/TnU9FSigjxk5xFp90xl68gDfggkE/wrlM=";
          };
        }
      ];
      shellAbbrs = import ./shell-aliases.nix;
      interactiveShellInit = "${pkgs.starship}/bin/starship init fish | source";
    };
  };
}
