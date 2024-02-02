{ pkgs, ... }: {
  imports = [ ../wm/x11.nix ];
  services.xserver = { desktopManager.gnome.enable = true; };
}
