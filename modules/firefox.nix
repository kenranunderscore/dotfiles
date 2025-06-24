{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.firefox.enable = lib.mkEnableOption "firefox";

  config = lib.mkIf config.my.firefox.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox-bin;
    };
  };
}
