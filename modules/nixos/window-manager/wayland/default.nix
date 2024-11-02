{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
let
  cfg = config.window-manager.wayland;
in
{
  options.window-manager.wayland = {
    enable = mkEnableOption "Wayland";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.wayland
      (pkgs.catppuccin-sddm.override {
        flavor = "mocha";
        font = "Cozette";
        fontSize = "9";
        loginBackground = true;
      })
    ];
    programs.sway.enable = true;

    environment.sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
    };

    security.polkit.enable = true;

    # TODO: extract opengl config
    hardware.graphics.enable = true;

    # TODO: extract display manager config
    services.xserver.enable = true;
    services.displayManager.sddm = {
      enable = true;
      wayland.enable = true;
      theme = "catppuccin-mocha";
      package = pkgs.kdePackages.sddm;
    };

    # TODO: extract keyring config
    # services.gnome.gnome-keyring.enable = true;
    # security.pam.services.sddm.enableGnomeKeyring = true;

    # TODO: extract out pipewire config
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    hardware.opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };
}
