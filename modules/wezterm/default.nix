{
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.wezterm;
  types = lib.types;
in
{
  options.modules.wezterm = {
    enable = lib.mkEnableOption "wezterm";
    withPackage = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = lib.optional cfg.withPackage pkgs.wezterm;
      activation.symlinkWeztermConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        config_dir="$XDG_CONFIG_HOME/wezterm"
        if [ ! -d $config_dir ]; then
          mkdir -p "$config_dir"
          $DRY_RUN_CMD ln -s $HOME/dotfiles/modules/wezterm/wezterm.lua $config_dir/
        fi
      '';
    };
  };
}
