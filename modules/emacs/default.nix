{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.emacs;
in
{
  options.my.emacs = {
    enable = lib.mkEnableOption "emacs";
    includePkg = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    symlink-config.files = [
      {
        source = ./emacs.d;
        destination = "emacs";
        xdg = true;
      }
    ];

    home.packages =
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
      lib.optionals cfg.includePkg [
        myEmacs

        # Programs needed at runtime or for straight to build packages
        cmake
        libtool
        meson
        ninja
        shellcheck
      ];
  };
}
