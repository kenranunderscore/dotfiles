{ config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.emacs;
  types = lib.types;
in {
  options.modules.programs.emacs = {
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
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/programs/emacs/emacs.d $HOME/.emacs.d
          fi
        '';
      };

      packages = let
        targetEmacs = if cfg.emacsVersion == "git" then
          pkgs.emacsGitNativeComp
        else
          pkgs.emacs;
        emacsWithPackages =
          (pkgs.emacsPackagesFor targetEmacs).emacsWithPackages;
        # Empty package list as I use straight.el now to try it out
        myEmacs = emacsWithPackages (_epkgs: [ ]);
      in with pkgs; [
        myEmacs

        # Programs needed at runtime or for straight to build packages
        gcc
        meson
        ninja
      ];
    };
  };
}
