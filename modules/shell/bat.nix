{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.bat;
in {
  options.modules.shell.bat = { enable = mkEnableOption "bat"; };

  config = mkIf cfg.enable {
    programs.bat = {
      enable = true;
      config.theme = "ansi";
    };
  };
}
