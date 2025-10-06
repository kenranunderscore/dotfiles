{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.neovim = {
    enable = lib.mkEnableOption "neovim";
    includePkg = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf config.my.neovim.enable {
    symlink-config.files = [
      {
        source = ./nvim;
        destination = "nvim";
        xdg = true;
      }
    ];

    home.packages =
      (with pkgs; [
        sumneko-lua-language-server
        stylua
        xclip
      ])
      ++ lib.optional config.my.neovim.includePkg pkgs.neovim;
  };
}
