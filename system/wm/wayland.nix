{ pkgs, ... }: {
  imports = [ ./pipewire.nix ../hardware/opengl.nix ];
  environment.systemPackages = [ pkgs.wayland ];
  services.xserver = {
    enable = true;
    layout = "us";
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };
  # Set up brightness keys
  programs.light.enable = true;
  # XDG Integration
  # XDG is a standard which formalises a set of common variables and methods for
  # desktop applications to interact with each other. The following settings
  # enable what's called an XDG Portal, which improves communication between
  # bundled flatpak apps, and Wayland
  # xdg.portal = {
  #   enable = true;
  #   extraPortals = with pkgs; [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk ];
  # };
}
