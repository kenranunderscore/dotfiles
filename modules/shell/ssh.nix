{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.ssh;
in {
  options.modules.shell.ssh.enable = lib.mkEnableOption "ssh";

  config = {
    programs.ssh = {
      enable = true;
      compression = true;

      matchBlocks = {
        "sync" = {
          host = "sync";
          hostname = "157.90.159.76";
          user = "kenran";
          compression = true;
        };

        "diablo" = {
          host = "diablo";
          hostname = "diablo";
          user = "ag";
          proxyCommand = "ssh maier@home.active-group.de -W %h:%p";
        };

        "diabloag" = {
          host = "diabloag";
          hostname = "diablo";
          user = "ag";
        };

        "cxc" = {
          host = "cxc";
          hostname = "crawl.xtahua.com";
          user = "crawl";
          compression = true;
        };

        "cdo" = {
          host = "cdo";
          hostname = "crawl.develz.org";
          user = "crawl";
          compression = true;
        };

        "cao" = {
          host = "cao";
          hostname = "crawl.akrasiac.org";
          user = "joshua";
          compression = true;
        };

        "cue" = {
          host = "cue";
          hostname = "underhound.eu";
          port = 23;
          user = "terminal";
          compression = true;
        };
      };
    };
  };
}
