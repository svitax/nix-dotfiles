# Declaratively configuring GNOME

# Most GNOME settings exist in 'dconf'. Most folks configure these via
# `gnome-settings' or the settings panels of the relevant applications.
# Run `dconf watch /' and set whatever option you're looking to declaratively
# persist, and observe the output:

# e.g.
# $ dconf watch /
# ...
# /org/gnome/desktop/interface/color-scheme
#    'default'
# /org/gnome/desktop/interface/color-scheme
#    'prefer-dark'

# Try setting those from the command line and observe the `gnome-settings'
# window change:
# dconf write /org/gnome/desktop/interface/color-scheme "'default'"
# dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"

# Using this information, we can add the following to the user configuration:
# dconf.settings = {
#   "org/gnome/desktop/interface" = {
#     color-scheme = "prefer-dark";
#   };
# };
{ config, pkgs, ... }: {
  # TODO: enable these extensions and apply settings from my Fedora box
  home.packages = with pkgs; [
    gnomeExtensions.appindicator # appindicatorsupport@gcjonas.gmail.com
    gnomeExtensions.arcmenu # arcmenu@arcmenu.com
    gnomeExtensions.bluetooth-quick-connect # bluetooth-quick-connect@bjarosze.gmail.com
    gnomeExtensions.brightness-control-using-ddcutil # display-brightness-ddcutil@themightydeity.github.com
    gnomeExtensions.caffeine # caffeine@patapon.info
    gnomeExtensions.dash-to-panel # dash-to-panel@jderose9.github.com
    gnomeExtensions.just-perfection # just-perfection-desktop@just-perfection
    gnomeExtensions.pop-shell # pop-shell@system76.com
    gnomeExtensions.rounded-window-corners # rounded-window-corners@yilozt
    gnomeExtensions.space-bar # space-bar@luchrioh
  ];
  dconf = {
    enable = true;
    # Use `dconf watch /' to strack stateful changes you are doing, then set
    # them here
    settings = {
      "org/gnome/shell" = {
        disable-user-extensions = false;
        # `gnome-extensions list' for a list
        enabled-extensions = [
          # "appindicatorsupport@gcjonas.gmail.com"
          "arcmenu@arcmenu.com"
          # "bluetooth-quick-connect@bjarosze.gmail.com"
          # "display-brightness-ddcutil@themightydeity.github.com"
          # "caffeine@patapon.info"
          # "dash-to-panel@jderose9.github.com"
          # "just-perfection-desktop@just-perfection"
          "pop-shell@system76.com"
          # "rounded-window-corners@yilozt"
          # "space-bar@luchrioh"
        ];
      };
      "org/gnome/shell/extensions/arcmenu" = { menulayout = "Eleven"; };
      "org/gnome/shell/extensions/pop-shell" = {
        tile-move-up-global = "<Shift><Super>k";
        tile-move-left-global = "<Shift><Super>h";
        tile-move-down-global = "<Shift><Super>j";
        tile-move-right-global = "<Shift><Super>l";
      };
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        enable-hot-corners = false;
      };
      "org/gnome/settings-daemon/plugins/media-keys" = {
        screensaver = [ "<Super>escape" ];
        search = [ "<super>space" ];
      };
      "org/gnome/desktop/wm/keybindings" = {
        minimize = [ ];
        close = [ "<Super>q" ];
      };
    };
  };
}
