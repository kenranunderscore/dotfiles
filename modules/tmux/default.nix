{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.tmux = {
    enable = lib.mkEnableOption "tmux";
  };

  config = lib.mkIf config.my.tmux.enable {
    symlink-config.files = [
      {
        source = ./tmux.conf;
        destination = "tmux/tmux.conf";
        xdg = true;
      }
    ];

    home.packages = [ pkgs.tmux ];
  };
}
