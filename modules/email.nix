{ config, lib, pkgs, ... }:

with lib;
let
  realName = "Johannes Maier";
  cfg = config.modules.email;
in with import <home-manager/modules/lib/dag.nix> { inherit lib; }; {
  options.modules.email = {
    enable = mkEnableOption "email module";

    maildir = mkOption {
      type = types.str;
      default = ".mail";
    };

    certificatesFile = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    primaryAccount = mkOption {
      type = types.enum [ "ag" "mailbox" ];
      default = "mailbox";
    };

    isSyncServer = mkOption {
      type = types.bool;
      default = false;
    };
  };

  # TODO notmuch configuration
  # TODO afew?

  config = mkIf cfg.enable {
    # TODO enable pass and automatically prompt for mail address entries
    accounts.email = {
      maildirBasePath = cfg.maildir;
      certificatesFile = cfg.certificatesFile;
      accounts = {
        mailbox = rec {
          address = "johannes.maier@mailbox.org";
          userName = address;
          primary = cfg.primaryAccount == "mailbox";
          mbsync = {
            enable = cfg.isSyncServer;
            create = "maildir";
            expunge = "both";
          };
          msmtp.enable = !cfg.isSyncServer;
          notmuch.enable = true;
          inherit realName;
          passwordCommand = "pass show email/johannes.maier@mailbox.org";
          imap = {
            host = "imap.mailbox.org";
            port = 993;
            tls.enable = true;
          };
          smtp = {
            host = "smtp.mailbox.org";
            port = 465;
            tls.enable = true;
          };
        };
        ag = {
          address = "johannes.maier@active-group.de";
          userName = "maier";
          primary = cfg.primaryAccount == "ag";
          mbsync = {
            enable = cfg.isSyncServer;
            create = "maildir";
            expunge = "both";
          };
          msmtp.enable = !cfg.isSyncServer;
          notmuch.enable = true;
          inherit realName;
          passwordCommand = "pass show email/johannes.maier@active-group.de";
          imap = {
            host = "imap.active-group.de";
            port = null;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };
          smtp = {
            host = "smtp.active-group.de";
            port = null;
          };
        };
      };
    };

    home.packages = with pkgs;
      [ muchsync ] ++ lib.optionals (!cfg.isSyncServer) [ notmuch ];

    programs.notmuch = {
      enable = true;
      # FIXME parameterize this, or at least use existing vars
      hooks = mkIf cfg.isSyncServer {
        preNew =
          "PASSWORD_STORE_DIR=/home/kenran/.local/share/password-store mbsync --all";
      };
    };

    services.muchsync = mkIf (!cfg.isSyncServer) {
      remotes.syncRoot = {
        remote.host = "157.90.159.76";
        # TODO pass Paln's username
        sshCommand = "ssh -l kenran -CTaxq";
        frequency = "*:0/2"; # every 2 minutes
      };
    };
  };
}
