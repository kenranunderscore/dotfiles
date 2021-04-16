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
        targetEmacs = if cfg.emacsVersion == "gcc" then
          pkgs.emacsGcc
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
            magit
            orderless
            projectile
            selectrum
            smartparens
            use-package

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
      rev = "119b923e4da2b716e46dce7aeade32576a282427";
      emacsOverlay = (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
      }));
    in [ emacsOverlay ];
  };
}
