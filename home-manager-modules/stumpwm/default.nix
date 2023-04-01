{ config, lib, pkgs, ... }:

let cfg = config.modules.stumpwm;
in {
  options.modules.stumpwm.enable = lib.mkEnableOption "stumpwm";

  config = lib.mkIf cfg.enable {
    home.activation = {
      symlinkDotStumpWM = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -e $HOME/.stumpwm.d ]; then
          $DRY_RUN_CMD ln -snf $HOME/dotfiles/home-manager-modules/stumpwm/stumpwm.d $HOME/.stumpwm.d
        fi
      '';
    };
  };
}
