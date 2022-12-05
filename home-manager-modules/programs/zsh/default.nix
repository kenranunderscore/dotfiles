{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.zsh;
in {
  options.modules.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf cfg.enable {
    # Packages that I want aliases to use, like 'exa' as 'ls'
    # replacement.
    home.packages = with pkgs; [ exa ];

    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      # I enable completion myself after the relevant fpath mutations.
      enableCompletion = false;
      # I choose to manage most of the plugins myself, by pinning the
      # sources and sourcing the files.  This gives better control
      # over the versions and makes the ordering explicit.  These
      # enable* options are sugar for enabling certain plugins, so
      # don't set them.
      enableSyntaxHighlighting = false;
      enableAutosuggestions = false;
      history = {
        path = "${config.xdg.dataHome}/zsh/zsh_history";
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
        unsetopt case_glob
        setopt prompt_subst
      '';
      initExtra = ''
        source ${inputs.zsh-autopair}/autopair.zsh
        autopair-init
        source ${inputs.zsh-autosuggestions}/zsh-autosuggestions.zsh
        source ${inputs.zsh-syntax-highlighting}/zsh-syntax-highlighting.zsh
        # Needs to be sourced _after_ z-sy-h
        source ${inputs.zsh-history-substring-search}/zsh-history-substring-search.zsh
        # Need to source zsh-abbr after z-sy-h, otherwise there's subtle
        # breakage with the way autosuggestions are highlighted (?).
        source ${inputs.zsh-abbr}/zsh-abbr.zsh

        # Autoload custom functions
        fpath+=$ZDOTDIR/functions
        autoload -Uz $ZDOTDIR/functions/*(:t)

        # Case-insensitive and in-word completion, for instance, complete
        # "cd ~/own<TAB>" to "cd ~/Downloads"
        zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*' 'm:{a-zA-Z}={A-Za-z}'

        # Highlight current selection when completing
        zstyle ':completion:*' menu select

        # Enable completion now that fpath is set
        autoload -Uz compinit && compinit

        # Key bindings
        bindkey '^[[A' history-substring-search-up
        bindkey '^[[B' history-substring-search-down

        source ${./prompt.zsh}
      '';
      localVariables = {
        # The one thing that's not as nice as in bash (but I don't have
        # it in fish either): I cannot really distinguish between
        # M-<backspace> and C-w, while in bash the former deletes
        # "file-wise" in a path, and the latter doesn't.  This makes it
        # so deletions stop at a / character.
        WORDCHARS = "\${WORDCHARS/\\/}";
        # When having entered part of a command and pressing
        # <up>/<down>, then the history should be searched for that
        # _prefix_ (zsh-history-substring-search).
        HISTORY_SUBSTRING_SEARCH_PREFIXED = "1";
        # Make sure to _never_ save duplicates in the history.  The
        # above home-manager setting only sets HISTORY_IGNORE_DUPS.
        HISTORY_IGNORE_ALL_DUPS = "1";
      };
      shellGlobalAliases = {
        ls = "exa";
        l = "exa -lbF --group-directories-first --icons";
        ll = "exa -lbGF --group-directories-first --icons";
        la = "exa -labF --group-directories-first --icons";
        lla = "exa -labGF --group-directories-first --icons";
      };
    };

    xdg.configFile = {
      # Create shell abbreviations (akin to what fish does) from the
      # set of shell aliases via zsh-abbr.
      "zsh/abbreviations".text =
        let aliases = import ../shell-aliases.nix { inherit pkgs; };
        in lib.concatStringsSep "\n"
        (lib.mapAttrsToList (alias: cmd: ''abbr -g ${alias}="${cmd}"'')
          aliases);

      # My custom functions live here.  They get added to fpath and
      # autoloaded as part of .zshrc above.
      "zsh/functions" = {
        source = ./functions;
        recursive = true;
      };
    };
  };
}
