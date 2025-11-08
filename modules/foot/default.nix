{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.foot = {
    enable = lib.mkEnableOption "foot";
    includePkg = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf config.my.foot.enable {
    symlink-config.files = [
      {
        source = ./foot.ini;
        destination = "foot/foot.ini";
        xdg = true;
      }
    ];

    home.packages = lib.optional config.my.foot.includePkg pkgs.foot;
  };
}
