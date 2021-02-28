{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.git;
in {
  options.modules.shell.git = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

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
      ignores = [ "*.swp" ];
      signing.signByDefault = !(isNull cfg.gpgKey);
      signing.key = cfg.gpgKey;
      extraConfig = {
        pull.rebase = "false";
        core.editor = "vim";
        init.defaultBranch = "main";
      };
    };
  };
}
