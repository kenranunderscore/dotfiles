{ ... }:

{
  # Any static configuration files that should be put into $XDG_CONFIG_HOME
  xdg.configFile = { "fourmolu/fourmolu.yaml".source = ./fourmolu.yaml; };

  # Some programs behave according to XDG conventions when certain environment
  # variables are set. Let's put all of those here; they're not really annoying
  # even if I don't use the corresponding programs much or at all anymore, but
  # prevent cluttering of $HOME in case I quickly try something.
  home.sessionVariables = { STACK_XDG = "1"; };
}
