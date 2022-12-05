{ config, lib, pkgs, ... }:

let
  cfg = config.modules.shell.pass;
  types = lib.types;
in {
  options.modules.shell.pass = {
    enable = lib.mkEnableOption "pass module";

    gpgKey = lib.mkOption {
      type = types.str;
      default = "";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (e: [ e.pass-import ]);
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
        PASSWORD_STORE_CLIP_TIME = "30";
        PASSWORD_STORE_ENABLE_EXTENSIONS = "true";
        PASSWORD_STORE_KEY = cfg.gpgKey;
      };
    };
  };
}
