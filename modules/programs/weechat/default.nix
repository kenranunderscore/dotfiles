{ config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.weechat;
  certPath = ../../../private/irssi.pem;
  dag = import <home-manager/modules/lib/dag.nix> { inherit lib; };
in with lib; {
  options.modules.programs.weechat = { enable = mkEnableOption "weechat"; };
  config = {
    home.file = {
      ".weechat" = {
        source = ../../../config/weechat;
        recursive = true;
      };
      ".weechat/irc.conf".text = import ./irc.conf.nix certPath;
    };
    home = { packages = [ pkgs.weechat ]; };
  };
}
