{ config, lib, pkgs, ... }:

with lib;
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption "emacs";

    emacsVersion = mkOption {
      type = types.enum [ "gcc" "unstable" "stable" ];
      default = "stable";
    };
  };

  config = mkIf cfg.enable {
    home = {
      activation = {
        # FIXME Check for existence of ~/.emacs.d
        symlinkDotEmacs = dagEntryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ln -snf ${builtins.toPath ./.}/.emacs.d $HOME/.emacs.d
        '';
      };

      packages = let
        #emacsWithPackages =
        #(pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages;
        myEmacs = pkgs.emacsWithPackages (epkgs:
          with epkgs.melpaPackages; [
            # Essential
            evil
            evil-collection
            general
            magit
            projectile
            selectrum
            smartparens
            use-package

            # Language-specific
            nix-mode
          ]);
      in [ myEmacs ];
    };

    nixpkgs.overlays = let
      rev = "e35ed9bf10b46e533e52add965926d00122c0620";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
