{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.wezterm;
in
{
  options.my.wezterm = {
    enable = lib.mkEnableOption "wezterm";
    withPackage = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./wezterm.lua;
        destination = "/wezterm/wezterm.lua";
        xdg = true;
      }
    ];
    home.packages = lib.optional cfg.withPackage pkgs.wezterm;
  };
}
