{ config, lib, pkgs, ... }:

let
  toggleKeyboardLayout = pkgs.writeShellScriptBin "toggle_de_us" ''
    active=$(setxkbmap -print | awk -F"+" '/xkb_symbols/ {print $2}')
    if [ "$active" = "de" ]; then
      echo "Enabling US layout..."
      setxkbmap us
    else
      echo "Enabling DE layout..."
      setxkbmap de
    fi
  '';
in {
  options.modules.programs.toggleKeyboardLayout = {
    enable = lib.mkEnableOption "toggleKeyboardLayout";
  };

  config = {
    home.packages = lib.mkIf config.modules.programs.toggleKeyboardLayout.enable
      [ pkgs.toggleKeyboardLayout ];
    nixpkgs.overlays = [ (_: _: { inherit toggleKeyboardLayout; }) ];
  };
}
