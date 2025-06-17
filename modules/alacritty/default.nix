{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.alacritty;
in
{
  options.my.alacritty = {
    enable = lib.mkEnableOption "alacritty";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.alacritty ];
    xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;
  };
}
