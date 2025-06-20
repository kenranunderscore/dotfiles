{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.neovim.enable = lib.mkEnableOption "neovim";

  config = lib.mkIf config.my.neovim.enable {
    symlink-config.files = [
      {
        source = ./nvim;
        destination = "nvim";
        xdg = true;
      }
    ];

    home.packages = with pkgs; [
      neovim
      sumneko-lua-language-server
      stylua
      xclip
    ];
  };
}
