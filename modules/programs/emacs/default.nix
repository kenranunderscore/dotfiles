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
            epkgs.elpaPackages.vertico
            evil
            gcmh
            general
            magit
            orderless
            smartparens
            use-package

            # Clojure
            cider
            clojure-mode

            # Common Lisp
            sly
            sly-asdf

            # CSV
            epkgs.csv-mode

            # Dhall
            dhall-mode

            # Docker
            dockerfile-mode

            # F#
            fsharp-mode

            # Fish
            fish-mode

            # Haskell
            haskell-mode

            # Ini
            ini-mode

            # Java
            meghanada

            # Markdown
            markdown-mode

            # Nim
            nim-mode

            # Nix
            nix-mode

            # OCaml
            merlin
            merlin-eldoc
            tuareg

            # Org
            epkgs.org
            org-appear
            org-bullets
            org-present
            org-roam

            # PlantUML
            plantuml-mode

            # PureScript
            purescript-mode
            psc-ide

            # Python
            anaconda-mode
            pyimport

            # Racket
            racket-mode

            # Rust
            rust-mode

            # YAML
            yaml-mode

            # Themes
            color-theme-sanityinc-tomorrow
            doom-themes

            # Utility
            ace-window
            all-the-icons
            all-the-icons-dired
            all-the-icons-ibuffer
            default-text-scale
            diminish
            diredfl
            editorconfig
            eglot
            envrc
            evil-cleverparens
            evil-collection
            evil-org
            evil-snipe
            evil-surround
            evil-visual-mark-mode
            git-gutter
            git-modes
            git-timemachine
            gnus-alias
            helpful
            hl-todo
            htmlize
            hydra
            lispy
            lispyville
            magit-todos
            marginalia
            epkgs.elpaPackages.rainbow-mode
            reformatter
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
