{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.emacs;
  types = lib.types;
in
{
  options.modules.emacs.enable = lib.mkEnableOption "emacs";

  config = lib.mkIf cfg.enable {
    home = {
      activation = {
        symlinkDotEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          dot_emacs="$XDG_CONFIG_HOME/emacs"
          if [ ! -e $dot_emacs ]; then
            $DRY_RUN_CMD ln -snf $HOME/dotfiles/modules/emacs/emacs.d $dot_emacs
          fi
        '';
      };

      packages =
        let
          emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs-git).emacsWithPackages;
          # Some packages should come "with Emacs" via nix, as they are either
          # notoriously difficult to build locally on NixOS, or not in MELPA etc.
          myEmacs = emacsWithPackages (p: [
            # (p.treesit-grammars.with-grammars (g: [
            #   g.tree-sitter-python
            #   g.tree-sitter-lua
            # ]))
            p.treesit-grammars.with-all-grammars
            p.vterm
          ]);
        in
        with pkgs;
        [
          myEmacs

          # Programs needed at runtime or for straight to build packages
          cmake
          gcc
          libtool
          meson
          ninja
          shellcheck
        ];
    };
  };
}
