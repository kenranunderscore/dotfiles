{
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.wezterm;
  types = lib.types;
in
{
  options.modules.wezterm = {
    enable = lib.mkEnableOption "wezterm";
    withPackage = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = lib.optional cfg.withPackage pkgs.wezterm;
    xdg.configFile."wezterm/wezterm.lua".source = ./wezterm.lua;
  };
}
