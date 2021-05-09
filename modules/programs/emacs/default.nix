{ config, lib, pkgs, ... }:

with lib;
with import <home-manager/modules/lib/dag.nix> { inherit lib; };
let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption "emacs";

    emacsVersion = mkOption {
      type = types.enum [ "unstable" "stable" "git" ];
      default = "stable";
    };

    nativeComp = mkOption {
      type = types.bool;
      default = false;
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
        targetEmacs = if cfg.emacsVersion == "git" then
          (pkgs.emacsGit.override { inherit (cfg) nativeComp; })
        else
          (if cfg.emacsVersion == "stable" then
            pkgs.emacs
          else
            pkgs.emacsUnstable);
        emacsWithPackages =
          (pkgs.emacsPackagesFor targetEmacs).emacsWithPackages;
        myEmacs = emacsWithPackages (epkgs:
          with epkgs.melpaPackages; [
            # Essential
            consult
            epkgs.elpaPackages.corfu
            evil
            general
            magit
            orderless
            projectile
            smartparens
            use-package
            epkgs.elpaPackages.vertico

            # Language-specific
            clojure-mode
            epkgs.csv-mode
            dhall-mode
            dockerfile-mode
            haskell-mode
            markdown-mode
            nix-mode
            epkgs.org
            org-bullets
            org-present
            plantuml-mode
            yaml-mode

            # Themes
            color-theme-modern
            epkgs.elpaPackages.modus-themes
            doom-themes

            # Utility
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
            diminish
            diredfl
            eglot
            envrc
            evil-collection
            evil-snipe
            evil-surround
            gnus-alias
            helpful
            hl-todo
            marginalia
            notmuch
            ripgrep
            switch-window
            which-key
          ]);
      in [ myEmacs ];
    };

    nixpkgs.overlays = let
      rev = "ea53ff1d30b005646f4efc086366e52d7fa23708";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
