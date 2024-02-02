{ pkgs, ... }: {
  imports = [ ./wayland.nix ];
  # programs.sway.enable = true;
  security.polkit.enable = true;
}
