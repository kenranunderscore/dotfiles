{ custom, config, lib, pkgs, ... }:

let
  cfg = config.modules.wezterm;
  types = lib.types;
in {
  options.modules.wezterm.enable = lib.mkEnableOption "wezterm";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.wezterm ];
    xdg.configFile."wezterm/wezterm.lua".source = ./wezterm.lua;
  };
}
