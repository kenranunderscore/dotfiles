{ config, lib, pkgs, ... }:

let cfg = config.modules.shell.ssh;
in {
  options.modules.shell.ssh.enable = lib.mkEnableOption "ssh";

  config = {
    programs.ssh = {
      enable = true;
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
      };
    };
  };
}
