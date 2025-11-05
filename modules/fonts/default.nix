{
  inputs,
  custom,
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs) callPackage;
  cfg = config.my.fonts;
in
{
  options.my.fonts = {
    enable = lib.mkEnableOption "fonts";

    withCustomBuilds = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = lib.mkForce true;

    home = {
      sessionVariables.KENRAN_DEFAULT_FONT = custom.font.name;

      packages =
        lib.optionals cfg.withCustomBuilds [
          (callPackage ./lucida-console.nix { })
          # (callPackage (import ./pp.nix "${inputs.privateConfig}") { })
          (callPackage ./termingus.nix { })
          (callPackage ./sf-mono.nix { inherit (inputs) sf-mono; })
        ]
        ++ (with pkgs; [
          agave
          anonymousPro
          cantarell-fonts
          cascadia-code
          corefonts
          courier-prime
          geist-font
          fantasque-sans-mono
          fira-code
          fixedsys-excelsior
          font-awesome_6
          hack-font
          ibm-plex
          inconsolata
          iosevka-bin
          iosevka-comfy.comfy
          (pkgs.iosevka.override {
            set = "Custom";
            privateBuildPlan = ''
              [buildPlans.IosevkaCustom]
              family = "Iosevka Custom"
              spacing = "normal"
              serifs = "slab"
              noCvSs = true
              exportGlyphNames = false

              [buildPlans.IosevkaCustom.weights.Regular]
              shape = 400
              menu = 400
              css = 400

              [buildPlans.IosevkaCustom.weights.Bold]
              shape = 700
              menu = 700
              css = 700
            '';
          })
          jetbrains-mono
          julia-mono
          maple-mono.truetype
          noto-fonts
          paratype-pt-mono
          roboto-mono
          source-code-pro
          ubuntu-sans-mono
          unifont
          vista-fonts
        ]);
    };
  };
}
