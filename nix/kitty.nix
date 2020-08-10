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
      font_size = if isDarwin then "20.0" else "12.0";
      adjust_line_height = 1;
      scrollback_lines = 50000;
      hide_window_decorations = true;
      remember_window_size = false;
      initial_window_width = 800;
      initial_window_height = 520;
      enable_audio_bell = false;
      # Base16 Atelier Estuary - kitty color config
      # Scheme by Bram de Haan (http://atelierbramdehaan.nl)
      background = "#22221b";
      foreground = "#929181";
      selection_background = "#929181";
      selection_foreground = "#22221b";
      url_color = "#878573";
      cursor = "#929181";
      active_border_color = "#6c6b5a";
      inactive_border_color = "#302f27";
      active_tab_background = "#22221b";
      active_tab_foreground = "#929181";
      inactive_tab_background = "#302f27";
      inactive_tab_foreground = "#878573";
      tab_bar_background = "#302f27";
      color0 = "#22221b";
      color1 = "#ba6236";
      color2 = "#7d9726";
      color3 = "#a5980d";
      color4 = "#36a166";
      color5 = "#5f9182";
      color6 = "#5b9d48";
      color7 = "#929181";
      color8 = "#6c6b5a";
      color9 = "#ae7313";
      color10 = "#302f27";
      color11 = "#5f5e4e";
      color12 = "#878573";
      color13 = "#e7e6df";
      color14 = "#9d6c7c";
      color15 = "#f4f3ec";
    };
  };
}
