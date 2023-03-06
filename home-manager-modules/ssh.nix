{ config, lib, pkgs, ... }:

let cfg = config.modules.ssh;
in {
  options.modules.ssh.enable = lib.mkEnableOption "ssh";

  config = {
    programs.ssh = {
      enable = true;
      compression = true;
      extraConfig = ''
        IdentityFile ~/.ssh/id_ed25519
        AddKeysToAgent yes
      '';

      matchBlocks = {
        "sync" = {
          host = "sync";
          hostname = "157.90.159.76";
          user = "kenran";
          compression = true;
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
