{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:

{
  options.my.alacritty = {
    enable = lib.mkEnableOption "alacritty";

    wrapWithNixGL = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = lib.mkIf config.my.alacritty.enable {
    symlink-config.files = [
      {
        source = ./alacritty.toml;
        destination = "alacritty/alacritty.toml";
        xdg = true;
      }
    ];
    home = {
      packages =
        let
          alacritty =
            if config.my.alacritty.wrapWithNixGL then
              inputs.self.lib.createNixGLWrapper pkgs pkgs.alacritty
            else
              pkgs.alacritty;
        in
        [ alacritty ];
      activation.downloadAlacrittyThemes = lib.hm.dag.entryAfter [ "symlinkCustomConfigFiles" ] ''
        target_dir="$XDG_CONFIG_HOME/alacritty/themes"
        if [ ! -d "$target_dir" ]; then
          $DRY_RUN_CMD ${lib.getExe pkgs.gitMinimal} clone \
                       --single-branch \
                       https://github.com/alacritty/alacritty-theme \
                       "$target_dir"
        else
          echo "Nothing to do"
        fi
      '';

    };
  };
}
