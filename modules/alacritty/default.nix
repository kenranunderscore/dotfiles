{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.alacritty.enable = lib.mkEnableOption "alacritty";

  config = lib.mkIf config.my.alacritty.enable {
    symlink-config.files = [
      {
        source = ./alacritty.toml;
        destination = "$XDG_CONFIG_HOME/alacritty/alacritty.toml";
      }
    ];
    home.packages = [ pkgs.alacritty ];
  };
}
