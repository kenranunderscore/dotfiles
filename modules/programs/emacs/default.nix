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
            embark
            embark-consult
            evil
            gcmh
            general
            magit
            orderless
            projectile
            smartparens
            use-package
            epkgs.elpaPackages.vertico

            # Language-specific
            cider
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
            racket-mode
            sly
            sly-asdf
            yaml-mode

            # Themes
            color-theme-modern
            epkgs.elpaPackages.modus-themes
            doom-themes

            # Utility
            ace-window
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
            default-text-scale
            diminish
            diredfl
            doom-modeline
            eglot
            envrc
            evil-cleverparens
            evil-collection
            evil-snipe
            evil-surround
            gnus-alias
            helpful
            hl-todo
            hydra
            marginalia
            notmuch
            ripgrep
            which-key
          ]);
      in [ myEmacs ];
    };

    nixpkgs.overlays = let
      rev = "2a8b1b2e9b680d7b682ff484b7d86ab00dd0c6dd";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
