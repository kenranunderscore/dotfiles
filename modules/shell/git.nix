{ config, lib, pkgs, ... }:

let
  cfg = config.modules.shell.git;
  types = lib.types;
in {
  options.modules.shell.git = {
    enable = lib.mkEnableOption "git";

    email = lib.mkOption {
      type = types.str;
      default = "";
    };

    gpgKey = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      package = pkgs.gitAndTools.gitFull;
      enable = true;
      userName = "Johannes Maier";
      userEmail = cfg.email;
      aliases = {
        co = "checkout";
        pushf = "push --force-with-lease";
      };
      ignores = [
        # Vim
        "*.swp"
        # Direnv
        ".direnv/"
        ".envrc"
        # macOS
        ".DS_Store"
        # Emacs: backup, auto-save, lock files, directory-local
        # variables
        "*~"
        "\\#*\\#"
        ".\\#*"
        ".dir-locals.el"
      ];
      signing.signByDefault = cfg.gpgKey != null;
      signing.key = if cfg.gpgKey != null then cfg.gpgKey else "";
      extraConfig = {
        core = {
          editor = "vim";
          askPass = "";
        };
        init.defaultBranch = "main";
        merge.conflictstyle = "diff3";
        pull.rebase = "true";
        submodule.recurse = "true";
        url = {
          "https://github.com/" = { insteadOf = "gh:"; };
          "git@github.com:kenranunderscore/" = { insteadOf = "gh:/"; };
          "https://gitlab.com/" = { insteadOf = "gl:"; };
          "ssh://git@gitlab.active-group.de:1022/ag/" = { insteadOf = "ag:"; };
        };
      };
    };
  };
}
