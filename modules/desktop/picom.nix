{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.picom;
in {
  options.modules.desktop.picom = { enable = lib.mkEnableOption "picom"; };

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      activeOpacity = "0.98";
      inactiveOpacity = "0.8";
      # menuOpacity = "0.8";
      backend = "glx";
      experimentalBackends = true;
      # fade = true;
      # fadeDelta = 5;
      # shadow = true;
      # shadowOpacity = "0.75";
      vSync = true;
      extraOptions = "xrender-sync-fence = true;";
    };
  };
}
