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

    nativeComp = lib.mkOption {
      type = types.bool;
      default = false;
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
            fsharp-mode
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
            gruber-darker-theme

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
            git-timemachine
            gnus-alias
            helpful
            hl-todo
            hydra
            lsp-mode
            magit-todos
            marginalia
            ripgrep
            vterm
            wgrep
            which-key
            yasnippet
          ]);
      in [ myEmacs ];
    };
  };
}
