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
          $DRY_RUN_CMD ln -snf ${builtins.toPath ./.}/emacs.d $HOME/.emacs.d
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
            company
            consult
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

            # Language-specific
            cider
            clojure-mode
            epkgs.csv-mode
            dhall-mode
            dockerfile-mode
            haskell-mode
            markdown-mode
            meghanada
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
            doom-themes

            # Utility
            ace-window
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
            default-text-scale
            diminish
            diredfl
            eglot
            envrc
            evil-cleverparens
            evil-collection
            evil-org
            evil-snipe
            evil-surround
            gnus-alias
            helpful
            hl-todo
            hydra
            magit-todos
            marginalia
            notmuch
            ol-notmuch
            ripgrep
            wgrep
            which-key
          ]);
      in [ myEmacs ];
    };

    nixpkgs.overlays = let
      rev = "88b0e77dec063bc6387952e6da323557fe666122";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
