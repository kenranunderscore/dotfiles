{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.qutebrowser;
in {
  options.modules.programs.qutebrowser = {
    enable = mkEnableOption "qutebrowser";

    package = mkOption {
      type = types.package;
      default = pkgs.qutebrowser;
    };
  };

  config = mkIf cfg.enable {
    programs.qutebrowser = {
      enable = true;
      package = cfg.package;
      searchEngines = {
        dict = "https://www.dict.cc/?s={}";
        g = "https://www.google.com/search?hl=en&q={}";
        h = "https://hoogle.haskell.org/?hoogle={}";
      };
    };
  };
}
