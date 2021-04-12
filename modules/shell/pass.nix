{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.pass;
in {
  options.modules.shell.pass = {
    enable = mkEnableOption "pass module";

    gpgKey = mkOption {
      type = types.str;
      default = "";
    };
  };

  config = mkIf cfg.enable {
    programs.password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (e: [ e.pass-import ]);
      settings = {
        PASSWORD_STORE_DIR = "~/.password-store";
        PASSWORD_STORE_CLIP_TIME = "30";
        PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
        PASSWORD_STORE_KEY = cfg.gpgKey;
      };
    };
  };
}
