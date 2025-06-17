{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.firefox;
in
{
  options.my.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox-beta-bin;
    };
  };
}
