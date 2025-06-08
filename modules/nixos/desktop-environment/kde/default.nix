{
  lib,
  config,
  ...
}:
let
  cfg = config.desktop-environment.kde;
in
{
  options.desktop-environment.kde = {
    enable = lib.mkEnableOption "Enables the KDE desktop environment";
  };

  config = lib.mkIf cfg.enable {

    # Enable the X11 windowing system.
    # You can disable this if you're only using the Wayland session.
    services.xserver.enable = true;

    # Enable the KDE Plasma Desktop Environment.
    services.displayManager.sddm.enable = true; # TODO extract displayManager config into module
    services.desktopManager.plasma6.enable = true;

    # Configure keymap in X11
    services.xserver.xkb = {
      layout = "us";
      variant = "";
    };
  };
}
