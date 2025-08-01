{ config, lib, ... }:

let
  types = lib.types;
  realName = "Johannes Maier";
  cfg = config.my.email;
in
{
  options.my.email = {
    enable = lib.mkEnableOption "email module";

    maildir = lib.mkOption {
      type = types.str;
      default = ".mail";
    };

    certificatesFile = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    primaryAccount = lib.mkOption {
      type = types.enum [
        "ag"
        "mailbox"
      ];
      default = "mailbox";
    };
  };

  config = lib.mkIf cfg.enable {
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
            enable = true;
            create = "both";
            remove = "both";
            expunge = "both";
            patterns = [
              "*"
              "!Drafts"
            ];
          };
          msmtp = {
            enable = true;
            extraConfig = {
              "syslog" = "LOG_USER";
            };
          };
          notmuch.enable = true;
          mu.enable = false;
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
        ag = rec {
          address = "johannes.maier@active-group.de";
          userName = address;
          primary = cfg.primaryAccount == "ag";
          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
            expunge = "both";
            patterns = [
              "*"
              "!Drafts"
              "!Deleted Messages"
            ];
          };
          msmtp = {
            enable = true;
            extraConfig = {
              "syslog" = "LOG_USER";
            };
          };
          notmuch.enable = true;
          mu.enable = false;
          inherit realName;
          passwordCommand = "pass show email/johannes.maier@active-group.de";
          imap = {
            host = "mail.active-group.de";
            port = null;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };
          smtp = {
            host = "mail.active-group.de";
            port = null;
          };
        };
      };
    };

    programs = {
      mbsync.enable = true;
      msmtp.enable = true;
      mu.enable = false;
      notmuch = {
        enable = true;
        hooks.preNew = "mbsync -a";
        maildir.synchronizeFlags = true;
        new.tags = [ ];
      };
    };
  };
}
