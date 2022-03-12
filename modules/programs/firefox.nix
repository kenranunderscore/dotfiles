{ config, lib, pkgs, ... }:

let cfg = config.modules.programs.firefox;
in {
  options.modules.programs.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox-beta-bin;
    };
  };
}
