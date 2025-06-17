{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.dwarfFortress;
in
{
  options.my.dwarfFortress = {
    enable = lib.mkEnableOption "dwarfFortress";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      let
        df = pkgs.dwarf-fortress-packages.dwarf-fortress-full.override { theme = null; };
      in
      with pkgs.dwarf-fortress-packages;
      [
        df
        soundSense
        dwarf-therapist
      ];
  };
}
