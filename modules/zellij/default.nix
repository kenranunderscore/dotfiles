{
  config,
  pkgs,
  lib,
  ...
}:

{
  options.my.zellij.enable = lib.mkEnableOption "zellij";

  config = lib.mkIf config.my.zellij.enable {
    home.packages = [ pkgs.zellij ];
    symlink-config.files = [
      {
        source = ./config.kdl;
        destination = "zellij/config.kdl";
        xdg = true;
      }
    ];
  };
}
