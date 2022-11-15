{ custom, config, lib, pkgs, ... }:

let cfg = config.modules.programs.alacritty;
in {
  options.modules.programs.alacritty = {
    enable = lib.mkEnableOption "alacritty";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.alacritty ];
    xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;
  };
}

