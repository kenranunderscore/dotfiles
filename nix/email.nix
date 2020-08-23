isDarwin:
{ lib, ... }:

let
  maildir = ".mail";
  realName = "Johannes Maier";
in with import <home-manager/modules/lib/dag.nix> { inherit lib; }; {
  accounts.email = {
    maildirBasePath = maildir;
    certificatesFile = if isDarwin then
      "/usr/local/etc/openssl/cert.pem"
    else
      "/etc/ssl/certs/ca-certificates.crt";
    accounts = {
      mailbox = rec {
        address = "johannes.maier@mailbox.org";
        userName = address;
        primary = !isDarwin;
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
        };
        msmtp.enable = true;
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
      gmail = {
        address = "johb.maier@gmail.com";
        flavor = "gmail.com";
        primary = false;
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
          patterns = [
            "*"
            "INBOX"
            "![Google Mail]*"
            "[Google Mail]/All Mail"
            "[Google Mail]/Trash"
            "[Google Mail]/Drafts"
            "[Google Mail]/Spam"
          ];
          flatten = ".";
          extraConfig = { account.PipelineDepth = 50; };
        };
        msmtp.enable = true;
        inherit realName;
        passwordCommand = "pass show email/johb.maier@gmail.com";
      };
      ag = {
        address = "johannes.maier@active-group.de";
        userName = "maier";
        primary = isDarwin;
        mbsync = {
          enable = true;
          create = "maildir";
          expunge = "both";
        };
        msmtp.enable = true;
        realName = "Johannes Maier";
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

  home.activation = {
    createMaildirIfNecessary =
      dagEntryAfter [ "writeBoundary" ] "$DRY_RUN_CMD mkdir -p ~/${maildir}";
  };
}
