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

    home.packages = [
      pkgs.kakoune
      pkgs.kak-lsp
      pkgs.kak-tree-sitter
    ];
  };
}
