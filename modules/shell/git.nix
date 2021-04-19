{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.git;
in {
  options.modules.shell.git = {
    enable = mkEnableOption "git";

    email = mkOption {
      type = types.str;
      default = "";
    };

    gpgKey = mkOption {
      type = types.nullOr types.str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      package = pkgs.gitAndTools.gitFull;
      enable = true;
      userName = "Johannes Maier";
      userEmail = cfg.email;
      ignores = [
        # Vim
        "*.swp"
        # Direnv
        ".envrc"
        # macOS
        ".DS_Store"
        # Emacs: backup, auto-save, lock files
        "*~"
        "\\#*\\#"
        ".\\#*"
      ];
      signing.signByDefault = cfg.gpgKey != null;
      signing.key = if cfg.gpgKey != null then cfg.gpgKey else "";
      extraConfig = {
        core = {
          editor = "vim";
          askPass = "";
        };
        init.defaultBranch = "main";
        pull.rebase = "true";
        url = {
          "https://github.com/" = { insteadOf = "gh:"; };
          "git@github.com:kenranunderscore/" = { insteadOf = "gh:/"; };
          "https://gitlab.com/" = { insteadOf = "gl:"; };
        };
      };
    };
  };
}
