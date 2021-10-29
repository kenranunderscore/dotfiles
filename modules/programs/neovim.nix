{ config, lib, pkgs, ... }:

let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim.enable = lib.mkEnableOption "neovim";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.neovim-nightly ];
    xdg.configFile = {
      "nvim" = {
        source = ../../config/nvim;
        recursive = true;
      };
    };
  };
}
