{
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.ghostty;
  types = lib.types;
in
{
  options.modules.ghostty = {
    enable = lib.mkEnableOption "ghostt";
    withPackage = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = lib.optional cfg.withPackage pkgs.ghostty;
      activation.symlinkGhosttyConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        config_dir="$XDG_CONFIG_HOME/ghostty"
        if [ ! -d $config_dir ]; then
          mkdir -p "$config_dir"
          $DRY_RUN_CMD ln -s $HOME/dotfiles/modules/ghostty/config $config_dir/
        fi
      '';
    };
  };
}
