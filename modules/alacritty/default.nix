{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.alacritty.enable = lib.mkEnableOption "alacritty";

  config = lib.mkIf config.my.alacritty.enable {
    symlink-config.files = [
      {
        source = ./alacritty.toml;
        destination = "alacritty/alacritty.toml";
        xdg = true;
      }
    ];
    home = {
      packages = [ pkgs.alacritty ];
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
