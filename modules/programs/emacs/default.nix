{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.emacs;
in {
  options.modules.programs.emacs = {
    enable = mkEnableOption "emacs";

    emacsVersion = mkOption {
      type = types.enum [ "stable" "git" ];
      default = "git";
    };

    nativeComp = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
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
          pkgs.emacsGit.override { inherit (cfg) nativeComp; }
        else
          pkgs.emacs;
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
            smartparens
            use-package

            # Language-specific
            anaconda-mode
            cider
            clojure-mode
            epkgs.csv-mode
            dhall-mode
            dockerfile-mode
            haskell-mode
            markdown-mode
            meghanada
            nim-mode
            nix-mode
            epkgs.org
            org-bullets
            org-present
            plantuml-mode
            pyimport
            racket-mode
            sly
            sly-asdf
            v-mode
            yaml-mode

            # Themes
            color-theme-modern
            color-theme-sanityinc-tomorrow
            doom-themes
            green-phosphor-theme
            humanoid-themes
            immaterial-theme

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
            git-gutter
            gnus-alias
            helpful
            hl-todo
            hydra
            magit-todos
            marginalia
            ripgrep
            wgrep
            which-key
          ]);
      in [ myEmacs ];
    };
  };
}
