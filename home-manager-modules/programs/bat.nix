{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.bat;
in {
  options.modules.shell.bat.enable = lib.mkEnableOption "bat";

  config = lib.mkIf cfg.enable {
    programs.bat = {
      enable = true;
      config.theme = "ansi";
    };
  };
}
