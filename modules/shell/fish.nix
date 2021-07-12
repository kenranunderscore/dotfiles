{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.fish;
in with lib; {
  options.modules.shell.fish = { enable = mkEnableOption "fish"; };

  config = mkIf cfg.enable {
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
            rev = "4f616a6071f05ae89d5f72c20216f6f851e6ca1d";
            sha256 = "1kr90b03pry7k74g80wnmmlw2d6pvp3637cifywqpkwdyizys6va";
          };
        }
        {
          name = "fish-async-prompt";
          src = pkgs.fetchFromGitHub {
            owner = "acomagu";
            repo = "fish-async-prompt";
            rev = "40f30a4048b81f03fa871942dcb1671ea0fe7a53";
            sha256 = "19i59145lsjmidqlgk2dmvs3vg2m3zlz2rcms2kyyk1m3y63q8xi";
          };
        }
        {
          name = "autopair.fish";
          src = pkgs.fetchFromGitHub {
            owner = "jorgebucaran";
            repo = "autopair.fish";
            rev = "1222311994a0730e53d8e922a759eeda815fcb62";
            sha256 = "0lxfy17r087q1lhaz5rivnklb74ky448llniagkz8fy393d8k9cp";
          };
        }
      ];
      shellAbbrs = import ./shell-aliases.nix;
      promptInit = "${pkgs.starship}/bin/starship init fish | source";
    };
  };
}
