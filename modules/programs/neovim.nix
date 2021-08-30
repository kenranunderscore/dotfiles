{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.neovim;
in {
  options.modules.programs.neovim = { enable = mkEnableOption "neovim"; };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.neovim-nightly ];
    xdg.configFile = {
      "nvim" = {
        source = ../../config/nvim;
        recursive = true;
      };
    };
  };
}
