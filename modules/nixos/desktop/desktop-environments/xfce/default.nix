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
    enable = lib.mkEnableOption "Enables the XFCE desktop environment";
  };

  config = lib.mkIf cfg.enable {

    # Enable the X11 windowing system.
    # You can disable this if you're only using the Wayland session.
    services.xserver.enable = true;

    # Enable the XFCE desktop environment.
    services.xserver.desktopManager.xfce.enable = true;

    environment.systemPackages = with pkgs; [ xfce.xfce4-pulseaudio-plugin ];
    programs.thunar.plugins = with pkgs; [
      xfce.thunar-archive-plugin
      xfce.thunar-dropbox-plugin
    ];

    services.blueman.enable = true;

    # Configure keymap in X11
    services.xserver.xkb = {
      layout = "us";
      variant = "";
    };
  };
}
