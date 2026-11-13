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
      settings = {
        "*" = {
          Compression = true;
          AddKeysToAgent = "yes";
        };

        "sync" = {
          HostName = "157.90.159.76";
          User = "kenran";
          Compression = true;
        };

        "cxc" = {
          HostName = "crawl.xtahua.com";
          User = "crawl";
          Compression = true;
        };

        "cdo" = {
          HostName = "crawl.develz.org";
          User = "crawl";
          Compression = true;
        };

        "cao" = {
          HostName = "crawl.akrasiac.org";
          User = "joshua";
          Compression = true;
        };

        "cue" = {
          HostName = "underhound.eu";
          Port = 23;
          User = "terminal";
          Compression = true;
        };

        "bosch-vpn" = {
          IdentityFile = "~/ag/bosch-vpn/docker/fl33rt@rt0vm670";
        };
      };
    };
  };
}
