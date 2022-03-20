{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    # TODO(Johannes):
    #
    # - select-word-style and other common options
    # - tabbing with completion -> highlight current match, fzf-tab
    # - in general: autoloading functions
    # - functions, how to autoload?
    # - ls colored, exa instead?
    # - prompt
    programs.zsh = let
      zsh-autopair = pkgs.fetchFromGitHub {
        owner = "hlissner";
        repo = "zsh-autopair";
        rev = "9d003fc02dbaa6db06e6b12e8c271398478e0b5d";
        sha256 = "sha256-hwZDbVo50kObLQxCa/wOZImjlH4ZaUI5W5eWs/2RnWg=";
      };
      zsh-abbr = pkgs.fetchFromGitHub {
        owner = "olets";
        repo = "zsh-abbr";
        rev = "91280150cf8de09f84ab02c00fc04605400ea914";
        sha256 = "sha256-6T27TTD4V3nzx1D8vhMpn2FIodYtLkOBoi6J7GYNV6k=";
      };
      zsh-syntax-highlighting = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "c5ce0014677a0f69a10b676b6038ad127f40c6b1";
        sha256 = "sha256-UqeK+xFcKMwdM62syL2xkV8jwkf/NWfubxOTtczWEwA=";
      };
      zsh-autosuggestions = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-autosuggestions";
        rev = "a411ef3e0992d4839f0732ebeb9823024afaaaa8";
        sha256 = "sha256-KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w=";
      };
    in {
      enable = true;
      dotDir = ".config/zsh";
      enableCompletion = true;
      # I chose to manager most of the plugins myself, by pinning the
      # sources (TODO: use flakes for this) and sourcing the files.
      # This gives better control over the versions and makes the
      # ordering explicit.
      enableSyntaxHighlighting = false;
      enableAutosuggestions = false;
      autocd = true;
      history = {
        size = 10000;
        save = 20000;
        share = true;
        expireDuplicatesFirst = true;
        ignoreDups = true;
        ignorePatterns = [ "rm *" "kill *" "pkill *" ];
      };
      initExtra = ''
        source ${zsh-autopair}/autopair.zsh
        source ${zsh-abbr}/zsh-abbr.zsh
        source ${zsh-syntax-highlighting}/zsh-syntax-highlighting.zsh
        source ${zsh-autosuggestions}/zsh-autosuggestions.zsh
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
