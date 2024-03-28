{ inputs, custom, config, lib, pkgs, ... }:

let
  inherit (pkgs) callPackage;
  cfg = config.modules.fonts;
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

    home = {
      sessionVariables.KENRAN_DEFAULT_FONT = custom.font.name;

      packages = lib.optionals cfg.withCustomBuilds [
        (callPackage ./lucida-console.nix { })
        (callPackage (import ./pp.nix "${inputs.privateConfig}") { })
        (callPackage ./termingus.nix { })
        (callPackage ./oldschool.nix { })
      ] ++ (with pkgs; [
        cantarell-fonts
        cascadia-code
        corefonts
        courier-prime
        geist-font
        fantasque-sans-mono
        fira-code
        font-awesome_6
        go-font
        hack-font
        hasklig
        ibm-plex
        inconsolata
        iosevka-bin
        jetbrains-mono
        julia-mono
        noto-fonts
        roboto-mono
        source-code-pro
        unifont
        victor-mono
        vistafonts
      ]);
    };
  };
}
