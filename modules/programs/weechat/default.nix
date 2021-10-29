{ inputs, config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.weechat;
  certPath = "${inputs.privateConfig}/irssi.pem";
in {
  options.modules.programs.weechat.enable = lib.mkEnableOption "weechat";

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
