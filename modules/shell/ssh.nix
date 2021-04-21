{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.ssh;
in {
  options.modules.shell.ssh = { enable = mkEnableOption "ssh"; };

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
      };
    };
  };
}
