{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.kakoune.enable = lib.mkEnableOption "kakoune";

  config = lib.mkIf config.my.kakoune.enable {
    symlink-config.files = [
      {
        source = ./kakrc;
        destination = "kak/kakrc";
        xdg = true;
      }
    ];

    home.packages =
      let
        k = pkgs.writeShellScriptBin "k" ''
          if [ -z "$TMUX" ]; then
            exec tmux new -As kak kak "$@"
          else
            exec kak "$@"
          fi
        '';
      in
      [
        k
        pkgs.kakoune
        pkgs.kak-lsp
        pkgs.kak-tree-sitter
      ];
  };
}
