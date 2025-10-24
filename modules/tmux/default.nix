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

    home = {
      activation = {
        installTmuxPluginManager = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          dest="$XDG_CONFIG_HOME/tmux/plugins/tpm"
          if [ ! -d "$dest" ]; then
            $DRY_RUN_CMD \
              ${lib.getExe pkgs.gitMinimal} clone \
              https://github.com/tmux-plugins/tpm \
              "$dest"
          fi
        '';
      };

      packages = [ pkgs.tmux ];
    };
  };
}
