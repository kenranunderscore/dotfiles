{ config, lib, pkgs, ... }:

let cfg = config.modules.fish;
in {
  options.modules.dcss.enable = lib.mkEnableOption "dcss";

  config = lib.mkIf cfg.enable {
    home = {
      file.".crawlrc".source = ./.crawlrc;
      packages = [ pkgs.crawl ];
    };
  };
}
