{ config, lib, ... }:

{
  options.my.ssh.enable = lib.mkEnableOption "ssh";

  config = {
    programs.ssh = lib.mkIf config.my.ssh.enable {
      enable = true;
      enableDefaultConfig = false;
      includes = [ "~/ag/bosch-vpn/ssh-config.d/*.conf" ];
      extraConfig = ''
        IdentityFile ~/.ssh/id_ed25519
      '';
      matchBlocks = {
        "*" = {
          compression = true;
          addKeysToAgent = "yes";
        };

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

        "bosch-vpn" = {
          identityFile = "~/ag/bosch-vpn/docker/fl33rt@rt0vm670";
        };
      };
    };
  };
}
