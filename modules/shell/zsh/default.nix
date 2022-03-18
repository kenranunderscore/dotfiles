{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    # TODO(Johannes):
    #
    # - ls colored
    # - prompt
    # - plugins from old zshrc
    # - tabbing with completion -> highlight current match
    # - in general: autoloading functions
    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      history = {
        size = 50000;
        share = true;
        expireDuplicatesFirst = true;
        ignoreDups = true;
        ignorePatterns = [ "rm *" "kill *" "pkill *" ];
      };
      plugins = [
        {
          name = "zsh-autopair";
          src = pkgs.fetchFromGitHub {
            owner = "hlissner";
            repo = "zsh-autopair";
            rev = "9d003fc02dbaa6db06e6b12e8c271398478e0b5d";
            sha256 = "sha256-hwZDbVo50kObLQxCa/wOZImjlH4ZaUI5W5eWs/2RnWg=";
          };
        }
        {
          name = "zsh-abbr";
          src = pkgs.fetchFromGitHub {
            owner = "olets";
            repo = "zsh-abbr";
            rev = "91280150cf8de09f84ab02c00fc04605400ea914";
            sha256 = "sha256-6T27TTD4V3nzx1D8vhMpn2FIodYtLkOBoi6J7GYNV6k=";
          };
        }
      ];
      # TODO: make this more bash-compatible, that is, use C-w for
      # deleting "whole words", and use M-BSPC for breaking on special
      # characters.
      envExtra = ''
        my-backward-kill-word() {
          local WORDCHARS='*?_-.[]~=&;!#$%^(){}<>:,"'"'"
          zle -f kill
          zle backward-kill-word
        }
        zle -N my-backward-kill-word
        bindkey '^w' my-backward-kill-word

        mc() {
          mkdir -p $1
          cd $1
        }
      '';
    };

    # Create shell abbreviations (akin to what fish does) from the set
    # of shell aliases.
    xdg.configFile = {
      "zsh/abbreviations".text = let aliases = import ../shell-aliases.nix;
      in lib.concatStringsSep "\n"
      (lib.mapAttrsToList (alias: cmd: ''abbr -g ${alias}="${cmd}"'') aliases);
    };
  };
}
