{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.my.dwarf-fortress = {
    enable = lib.mkEnableOption "dwarf-fortress";
  };

  config = lib.mkIf config.my.dwarf-fortress.enable {
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
