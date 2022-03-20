{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    # Packages that I want aliases to use.
    home.packages = with pkgs; [ exa ];

    # TODO(Johannes):
    # - functions, how to autoload?
    # - prompt
    # - flakes for pinning plugins
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
      # I choose to manage most of the plugins myself, by pinning the
      # sources and sourcing the files.  This gives better control
      # over the versions and makes the ordering explicit.  These
      # enable* options are sugar for enabling certain plugins, so
      # don't set them.
      enableSyntaxHighlighting = false;
      enableAutosuggestions = false;
      history = {
        size = 10000;
        save = 20000;
        share = true;
        expireDuplicatesFirst = true;
        ignoreDups = true;
        ignorePatterns = [ "rm *" "kill *" "pkill *" ];
      };
      # I know autocd is an option in programs.zsh but -- as with
      # plugins and variables -- I don't like mixing methods.
      initExtraFirst = ''
        setopt auto_cd
      '';
      initExtra = ''
        source ${zsh-autopair}/autopair.zsh
        autopair-init
        source ${zsh-autosuggestions}/zsh-autosuggestions.zsh
        source ${zsh-syntax-highlighting}/zsh-syntax-highlighting.zsh
        # Need to source zsh-abbr after z-sy-h, otherwise there's subtle
        # breakage with the way autosuggestions are highlighted (?).
        source ${zsh-abbr}/zsh-abbr.zsh

        # Highlight current selection when completing
        zstyle ':completion:*' menu select
      '';
      # The one thing that's not as nice as in bash (but I don't have
      # it in fish either): I cannot really distinguish between
      # M-<backspace> and C-w, while in bash the former deletes
      # "file-wise" in a path, and the latter doesn't.  This makes it
      # so deletions stop at a / character.
      localVariables = { WORDCHARS = "\${WORDCHARS/\\/}"; };
      shellGlobalAliases = {
        ls = "exa";
        l = "exa -lbF --git --group-directories-first --icons";
        ll = "exa -lbGF --git --group-directories-first --icons";
        la = "exa -labF --git --group-directories-first --icons";
        lla = "exa -labGF --git --group-directories-first --icons";
      };
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
