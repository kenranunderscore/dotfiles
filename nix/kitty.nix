isDarwin: shellPath: # TODO make this a module so we can configure from the outside
{ pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.hack-font;
      name = "Hack";
    };
    settings = {
      shell = shellPath + (if isDarwin then " --login" else "");
      macos_option_as_alt = true;
      font_size = if isDarwin then "17.0" else "12.0";
      adjust_line_height = 1;
      scrollback_lines = 50000;
      hide_window_decorations = true;
      remember_window_size = false;
      initial_window_width = 800;
      initial_window_height = 520;
      enable_audio_bell = false;
      # Color scheme
      background = "#162126";
      foreground = "#a6b5b5";
      selection_background = "#a6b5b5";
      selection_foreground = "#162126";
      url_color = "#878573";
      cursor = "#929181";
      active_border_color = "#6c6b5a";
      inactive_border_color = "#302f27";
      active_tab_background = "#22221b";
      active_tab_foreground = "#929181";
      inactive_tab_background = "#302f27";
      inactive_tab_foreground = "#878573";
      tab_bar_background = "#302f27";
      # black
      color0 = "#070b0d";
      color8 = "#0f161a";
      # red
      color1 = "#f72c25";
      color9 = "#f72c25";
      # green
      color2 = "#6bde38";
      color10 = "#6bde38";
      # yellow
      color3 = "#fcca46";
      color11 = "#fcca46";
      # blue
      color4 = "#1b98e0";
      color12 = "#1b98e0";
      # magenta
      color5 = "#f374ae";
      color13 = "#f374ae";
      # cyan
      color6 = "#46d9ff";
      color14 = "#46d9ff";
      # white
      color7 = "#a6b5b5";
      color15 = "#a6b5b5";
    };
  };
}
