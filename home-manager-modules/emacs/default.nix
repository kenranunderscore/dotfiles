{ config, lib, pkgs, ... }:

let
  cfg = config.modules.emacs;
  types = lib.types;
in {
  options.modules.emacs = {
    enable = lib.mkEnableOption "emacs";

    emacsVersion = lib.mkOption {
      type = types.enum [ "stable" "git" ];
      default = "git";
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkDotEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          if [ ! -e $HOME/.emacs.d ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/home-manager-modules/emacs/emacs.d $HOME/.emacs.d
          fi
        '';
      };

      packages = let
        targetEmacs = if cfg.emacsVersion == "git" then
          pkgs.emacsGit
        else
          pkgs.emacs;
        emacsWithPackages =
          (pkgs.emacsPackagesFor targetEmacs).emacsWithPackages;
        # Empty package list as I use straight.el now to try it out.
        # vterm is an exception as it does not currently build in a
        # naive way, which makes straight.el support difficult.
        myEmacs = emacsWithPackages (p: [ p.vterm ]);
      in with pkgs; [
        myEmacs

        # Programs needed at runtime or for straight to build packages
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
