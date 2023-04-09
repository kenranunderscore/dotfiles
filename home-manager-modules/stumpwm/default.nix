{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.stumpwm;
in {
  options.modules.stumpwm.enable = lib.mkEnableOption "stumpwm";

  config = lib.mkIf cfg.enable {
    home.activation = let stumpDir = "$HOME/.stumpwm.d";
    in {
      symlinkDotStumpWM = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -e ${stumpDir} ]; then
          $DRY_RUN_CMD ln -snf $HOME/dotfiles/home-manager-modules/stumpwm/stumpwm.d ${stumpDir}
        fi
      '';
      symlinkStumpWMContrib = lib.hm.dag.entryAfter [ "symlinkDotStumpWM" ] ''
        contribDir=${stumpDir}/modules/stumpwm-contrib
        if [ ! -e $contribDir ]; then
          $DRY_RUN_CMD mkdir -p ${stumpDir}/modules
          $DRY_RUN_CMD ln -snf ${inputs.stumpwm-contrib} $contribDir
        fi
      '';
    };
  };
}
