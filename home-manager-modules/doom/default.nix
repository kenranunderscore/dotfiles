{ config, lib, pkgs, ... }:

let
  cfg = config.modules.doom;
  types = lib.types;
in {
  options.modules.doom = {
    enable = lib.mkEnableOption "doom";

    emacsVersion = lib.mkOption {
      type = types.enum [ "stable" "git" ];
      default = "git";
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
            $DRY_RUN_CMD ${lib.getExe pkgs.gitMinimal} clone \
                         --depth=1 \
                         --single-branch \
                         https://github.com/doomemacs/doomemacs \
                         "$XDG_CONFIG_HOME/emacs"
          fi
          if [ ! -e "$XDG_CONFIG_HOME/doom" ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/home-manager-modules/doom/doom \
                         "$XDG_CONFIG_HOME/doom"
          fi
        '';
      };

      packages = let
        targetEmacs =
          if cfg.emacsVersion == "git" then pkgs.emacs-git else pkgs.emacs;
      in with pkgs; [
        targetEmacs

        # Programs needed at runtime
        cmake
        gcc
        libtool
        meson
        ninja
        shellcheck
      ];
    };
  };
}
