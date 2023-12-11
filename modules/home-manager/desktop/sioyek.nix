{config, ...}: let
  inherit (config.colorScheme) colors;
in {
  programs.sioyek = {
    enable = true;
    config = {
      custom_background_color = "#${colors.base00}";
      custom_text_color = "#${colors.base05}";

      page_separator_color = "#${colors.base01}";
      search_highlight_color = "#${colors.base0A}";
      status_bar_color = "#${colors.base00}";
      status_bar_text_color = "#${colors.base06}";
      ui_text_color = "#${colors.base06}";
      ui_selected_text_color = "#${colors.base06}";
      ui_background_color = "#${colors.base01}";
      ui_selected_background_color = "#${colors.base03}";
      background_color = "#${colors.base00}";
      visual_mark_color = "0.4 0.36078432 0.32941177 0.2";
      text_highlight_color = "#${colors.base03}";
      link_highlight_color = "#${colors.base0D}";
      synctex_highlight_color = "#${colors.base08}";

      page_separator_width = "10";

      startup_commands = "toggle_custom_color";
      ruler_mode = "1";
      font_size = "20";
      should_launch_new_window = "1";
      super_fast_search = "1";
      case_sensitive_search = "0";
      show_document_name_in_statusbar = "1";
    };
    bindings = {
      move_right = "h";
      move_down = "j";
      move_up = "k";
      move_left = "l";
      move_visual_mark_up = "<up>";
      move_visual_mark_down = "<down>";
      next_page = "J";
      previous_page = "K";
      next_state = "<C-i>";
      prev_state = "<C-o>";
      goto_definition = "gd";
      add_highlight = "ah";
      fit_to_page_width_smart = "aa";
      toggle_custom_color = "dd";
      screen_down = "<C-d>";
      screen_up = "<C-u>";
      goto_left_smart = "gh";
      goto_right_smart = "gl";
      new_window = "<C-t>";
    };
  };
}
