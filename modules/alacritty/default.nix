{
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.alacritty;
in
{
  options.modules.alacritty = {
    enable = lib.mkEnableOption "alacritty";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.alacritty ];
    xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;
  };
}
