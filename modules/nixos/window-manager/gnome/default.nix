{
  pkgs,
  lib,
  config,
  ...
}: let
  cfg = config.window-manager.gnome;
in {
  options.window-manager.gnome = {
    enable = lib.mkEnableOption "Enables the GNOME desktop environment";
  };

  config = lib.mkIf cfg.enable {
    # TODO: extract pipewire config
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    services.xserver = {
      enable = true;
      layout = "us";
      videoDrivers = ["amdgpu" "radeon"];
      # TODO: extract display manager config
      displayManager.gdm = {
        enable = true;
      };
      # TODO: extract desktop manager config?
      desktopManager.gnome.enable = true;
    };
  };
}
