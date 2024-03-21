{ config, lib, pkgs, ... }:

let
  cfg = config.modules.doom;
  types = lib.types;
in {
  options.modules.doom.enable = lib.mkEnableOption "doom";

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
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/doom/doom \
                         "$XDG_CONFIG_HOME/doom"
          fi
        '';
      };

      packages = let
        emacsWithPackages =
          (pkgs.emacsPackagesFor pkgs.emacs29).emacsWithPackages;
        # Doom manages packages itself, but vterm is an exception as it
        # sometimes does not build in a naive way. Also have Emacs know all
        # treesit grammars by default, so we don't have to install them
        # externally later on.
        myEmacs = emacsWithPackages
          (p: [ p.vterm p.treesit-grammars.with-all-grammars p.mu4e ]);
      in with pkgs; [
        myEmacs

        # Programs needed at runtime
        cmake
        fd
        gcc
        libtool
        meson
        ninja
        ripgrep
        shellcheck
      ];
    };
  };
}
