{ config, ... }:
let inherit (config.colorScheme) colors;
in {
  home.file.".config/kitty/pager.sh".source = ./pager.sh;
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 16;
    };
    settings = {
      # hide_window_decorations = true;
      wayland_titlebar_color = "#${config.colorScheme.colors.base00}";
      scrollback_lines = 10000;
      tab_bar_style = "hidden";
      scrollback_pager = "~/.config/kitty/pager.sh";

      # Colorscheme
      foreground = "#${colors.base05}";
      background = "#${colors.base00}";
      selection_foreground = "#${colors.base05}";
      selection_background = "#${colors.base00}";
      url_color = "#${colors.base04}";
      cursor = "#${colors.base05}";
      active_border_color = "#${colors.base03}";
      inactive_border_color = "#${colors.base01}";
      color0 = "#${colors.base00}";
      color1 = "#${colors.base08}";
      color2 = "#${colors.base0B}";
      color3 = "#${colors.base0A}";
      color4 = "#${colors.base0D}";
      color5 = "#${colors.base0E}";
      color6 = "#${colors.base0C}";
      color7 = "#${colors.base05}";
      color8 = "#${colors.base03}";
      color9 = "#${colors.base08}";
      color10 = "#${colors.base0B}";
      color11 = "#${colors.base0A}";
      color12 = "#${colors.base0D}";
      color13 = "#${colors.base0E}";
      color14 = "#${colors.base0C}";
      color15 = "#${colors.base07}";
      color16 = "#${colors.base09}";
      color17 = "#${colors.base0F}";
      color18 = "#${colors.base01}";
      color19 = "#${colors.base02}";
      color20 = "#${colors.base04}";
      color21 = "#${colors.base06}";
    };
    extraConfig = ''
      modify_font underline_position +2
      modify_font underline_thickness 2px
    '';
  };
}
