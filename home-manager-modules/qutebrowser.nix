{ config, lib, pkgs, ... }:

let
  cfg = config.modules.qutebrowser;
  types = lib.types;
in {
  options.modules.qutebrowser = {
    enable = lib.mkEnableOption "qutebrowser";

    package = lib.mkOption {
      type = types.package;
      default = pkgs.qutebrowser;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.qutebrowser = {
      enable = true;
      package = cfg.package;
      searchEngines = {
        dict = "https://www.dict.cc/?s={}";
        g = "https://www.google.com/search?hl=en&q={}";
        h = "https://hoogle.haskell.org/?hoogle={}";
        yt = "youtube.com/results?search_query={}";
      };
    };
  };
}
