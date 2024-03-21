{ config, lib, pkgs, ... }:

let cfg = config.modules.nyxt;
in {
  options.modules.nyxt.enable = lib.mkEnableOption "nyxt";

  config = lib.mkIf cfg.enable {
    home = {
      packages = [ pkgs.nyxt ];
      activation.symlinkNyxtConfig =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          config_dir=$XDG_CONFIG_HOME/nyxt

          if [ ! -d "$config_dir" ]; then
            mkdir -p "$(dirname "$config_dir")"
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/nyxt/nyxt \
                         "$config_dir"
          fi
        '';
    };
  };
}
