{ pkgs, ... }: {
  imports = [ ./wayland.nix ./swaylock.nix ];
  # programs.sway.enable = true;
  security.polkit.enable = true;

  environment.etc."wallpaper.jpg".source = ../style/wallpaper7.png;

}
