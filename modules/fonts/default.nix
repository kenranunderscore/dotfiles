{ config, lib, pkgs, ... }:

let cfg = config.modules.fonts;
in {
  options.modules.fonts = {
    enable = lib.mkEnableOption "fonts";

    withCustomBuilds = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = lib.mkForce true;

    home.packages = with pkgs; [
      anonymousPro
      camingo-code
      cantarell_fonts
      cascadia-code
      fira-code
      go-font
      hack-font
      hasklig
      hermit
      ibm-plex
      inconsolata
      (iosevka.override {
        privateBuildPlan = {
          family = "Iosevka Custom";
          spacing = "normal";
          serifs = "slab";
          ligations.inherits = "haskell";
          slopes = {
            upright = {
              angle = 0;
              shape = "upright";
              menu = "upright";
              css = "normal";
            };
            italic = {
              angle = 9.4;
              shape = "oblique";
              menu = "italic";
              css = "italic";
            };
            oblique = {
              angle = 9.4;
              shape = "oblique";
              menu = "oblique";
              css = "oblique";
            };
          };
        };
        set = "iosevka-custom";
      })
      jetbrains-mono
      meslo-lg
      nerdfonts
      roboto-mono
      source-code-pro
      terminus_font
      ubuntu_font_family
      unifont
      uw-ttyp0
    ];
  };
}
