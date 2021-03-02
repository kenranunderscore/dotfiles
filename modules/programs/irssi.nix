{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.irssi;
in {
  options.modules.programs.irssi = { enable = mkEnableOption "irssi"; };

  config = mkIf cfg.enable {
    home.file.".irssi/h3rbz.theme".source = ../../config/h3rbz.theme;
    programs.irssi = {
      enable = true;
      # TODO make cert part of options
      extraConfig = ''
        servers = (
          {
            address = "chat.freenode.net";
            chatnet = "freenode";
            port = "6697";
            use_tls = "yes";
            tls_cert = "${../../private/irssi.pem}";
            tls_verify = "no";
            autoconnect = "yes";
          }
        );

        chatnets = { freenode = { type = "IRC"; }; };

        channels = (
          { name = "#linux"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#nixos"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#emacs"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#neovim"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#org-mode"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#home-manager"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#zsh"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#haskell"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#lisp"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#nim"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "#zig"; chatnet = "freenode"; autojoin = "yes"; },
          { name = "##crawl"; chatnet = "freenode"; autojoin = "yes"; }
        );

        settings = {
          "fe-common/core" = {
            theme = "h3rbz";
          };
        };
      '';
    };
  };
}
