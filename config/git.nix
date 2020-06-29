pkgs: isDarwin: {
  package = pkgs.gitAndTools.gitFull;
  enable = true;
  userName = "Johannes Maier";
  userEmail = if isDarwin then
    "johannes.maier@active-group.de"
  else
    "johb.maier@gmail.com";
  ignores = [ "*.swp" ];
  signing.signByDefault = true;
  signing.key = if isDarwin then
    "0x4DC80C3B727DC1EE"
  else
    "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
  extraConfig = {
    pull.rebase = "false";
    core.editor = "vim";
  };
}
