{ config, pkgs, ... }: {
  # TODO: enable these extensions and apply settings from my Fedora box
  home.packages = with pkgs; [
    gnomeExtensions.appindicator
    gnomeExtensions.arcmenu
    gnomeExtensions.bluetooth-quick-connect
    gnomeExtensions.brightness-control-using-ddcutil
    gnomeExtensions.caffeine
    gnomeExtensions.dash-to-panel
    gnomeExtensions.just-perfection
    gnomeExtensions.pop-shell
    gnomeExtensions.rounded-window-corners
    gnomeExtensions.space-bar
  ];
  dconf = {
    enable = true;
    settings = {
      "org/gnome/shell" = {
        disable-user-extensions = false;
        enabled-extensions = [ "pop-shell@system76.com" "arcmenu@arcmenu.com" ];
      };
      "org/gnome/shell/extensions/arcmenu" = { menulayout = "Eleven"; };
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        enable-hot-corners = false;
      };
      "org/gnome/settings-daemon/plugins/media-keys" = {
        screensaver = [ ];
        search = [ "<Super>space" ];
      };
      "org/gnome/desktop/wm/keybindings" = {
        minimize = [ ];
        close = [ "<Super>q" ];
      };
    };
  };
}
