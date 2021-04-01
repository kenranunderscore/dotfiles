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
            company
            magit
            orderless
            projectile
            selectrum
            smartparens
            use-package

            # Language-specific
            dhall-mode
            dockerfile-mode
            haskell-mode
            markdown-mode
            nix-mode
            epkgs.org
            org-bullets
            org-present
            yaml-mode

            # Themes
            modus-themes
            doom-themes

            # Utility
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
            diminish
            envrc
            helpful
            hl-todo
            marginalia
            switch-window
            which-key
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
