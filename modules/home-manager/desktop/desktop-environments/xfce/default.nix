{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.modules.desktop.desktop-environments.xfce;
in
{
  options.modules.desktop.desktop-environments.xfce = {
    enable = lib.mkEnableOption "Configuration for the XFCE desktop environment";
  };

  config = lib.mkIf cfg.enable {
    xfconf.settings = {
      xfce4-keyboard-shortcuts = {
        "commands/custom/<Super>d" = "xfce4-appfinder";
        "xfwm4/custom/<Primary><Alt>d" = false; # unmap show_desktop_key
        "xfwm4/custom/<Alt>space" = false; # unmap popup_menu_key
      };
      xsettings = {
        "Gtk/FontName" = "Aporetic Sans 14";
        "Gtk/MonospaceFontName" = "Aporetic Sans Mono 14";
        "Xfce/LastCustomDPI" = 109;
        "Xft/DPI" = 109;
      };
      xfwm4 = {
        "general/title_font" = "Aporetic Sans Bold 12";
        "general/workspace_count" = 4;
        "general/workspace_names" = ["1" "2" "3" "4"];
      };
      xfce4-panel = {
        "panels" = [ 1 ]; # Remove Panel 2 (the "dock-like" bottom panel)
        "panels/panel-1/plugin-ids" = [1 2 3 4 5 6 7 8 9 10 11];
        "panels/panel-1/position" = "p=10;x=0;y=0"; # Move Panel 1 to the bottom
        "panels/panel-1/size" = 38;
        "panels/panel-1/icon-size" = 0; # Icons adjust size automatically
        "plugins/plugin-1" = "applicationsmenu";
        "plugins/plugin-1/show-button-title" = false;
        "plugins/plugin-2" = "tasklist";
        "plugins/plugin-2/grouping" = false;
        "plugins/plugin-3" = "separator";
        "plugins/plugin-3/expand" = true;
        "plugins/plugin-4" = "pager";
        "plugins/plugin-4/miniature-view" = false;
        "plugins/plugin-5" = "separator";
        "plugins/plugin-6" = "systray";
        "plugins/plugin-6/icon-size" = 0; # Icons adjust size automatically
        "plugins/plugin-7" = "pulseaudio";
        "plugins/plugin-8" = "separator";
        "plugins/plugin-9" = "clock";
        "plugins/plugin-9/digital-date-font" = "Sans Bold 10";
        "plugins/plugin-9/digital-time-font" = "Sans Bold 10";
        "plugins/plugin-10" = "separator";
        "plugins/plugin-11" = "actions";
      };
      xfce4-screensaver = {
        "lock/saver-activation-delay" = 10;
      };
      keyboards = {
        "Default/KeyRepeat/Delay" = 350;
        "Default/KeyRepeat/Rate" = 35;
      };
      thunar = {
        "last-view" = "ThunarCompactView";
      };
    };
  };
}
