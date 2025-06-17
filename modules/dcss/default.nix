{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.fish;
in
{
  options.my.dcss.enable = lib.mkEnableOption "dcss";

  config = lib.mkIf cfg.enable {
    home = {
      file.".crawlrc".source = ./.crawlrc;
      packages = [ pkgs.crawl ];
    };
  };
}
