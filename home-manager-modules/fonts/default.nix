{ inputs, custom, config, lib, pkgs, ... }:

let
  cfg = config.modules.fonts;
  pragmataPro =
    pkgs.callPackage (import ./pp.nix "${inputs.privateConfig}") { };
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
        (import ./sf-mono.nix {
          inherit (pkgs) runCommand;
          inherit (inputs) sf-mono;
        })
        (import ./lucida-console.nix { inherit (pkgs) runCommand fetchurl; })
        pragmataPro
      ] ++ (with pkgs; [
        borg-sans-mono
        cantarell-fonts
        cascadia-code
        courier-prime
        fantasque-sans-mono
        fira-code
        font-awesome_6
        go-font
        hack-font
        hasklig
        ibm-plex
        inconsolata
        iosevka
        jetbrains-mono
        julia-mono
        noto-fonts
        roboto-mono
        source-code-pro
        terminus_font_ttf
        victor-mono
      ]);
    };
  };
}
