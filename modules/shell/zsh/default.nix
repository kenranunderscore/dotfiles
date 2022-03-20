{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.shell.zsh;
in {
  options.modules.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    # Packages that I want aliases to use, like 'exa' as 'ls'
    # replacement.
    home.packages = with pkgs; [ exa ];

    # TODO(Johannes):
    # - prompt
    # - case insensitive globbing
    # - completion of word parts
    programs.zsh = let
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
        source ${inputs.zsh-autopair}/autopair.zsh
        autopair-init
        source ${inputs.zsh-autosuggestions}/zsh-autosuggestions.zsh
        source ${inputs.zsh-syntax-highlighting}/zsh-syntax-highlighting.zsh
        # Need to source zsh-abbr after z-sy-h, otherwise there's subtle
        # breakage with the way autosuggestions are highlighted (?).
        source ${inputs.zsh-abbr}/zsh-abbr.zsh

        # Highlight current selection when completing
        zstyle ':completion:*' menu select

        # Autoload custom functions
        fpath+=$ZDOTDIR/functions
        autoload -Uz $ZDOTDIR/functions/*(:t)
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

    xdg.configFile = {
      # Create shell abbreviations (akin to what fish does) from the
      # set of shell aliases via zsh-abbr.
      "zsh/abbreviations".text = let aliases = import ../shell-aliases.nix;
      in lib.concatStringsSep "\n"
      (lib.mapAttrsToList (alias: cmd: ''abbr -g ${alias}="${cmd}"'') aliases);

      # My custom functions live here.  They get added to fpath and
      # autoloaded as part of .zshrc above.
      "zsh/functions" = {
        source = ./functions;
        recursive = true;
      };
    };
  };
}
