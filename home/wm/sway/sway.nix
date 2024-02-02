{ config, lib, pkgs, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4"; # Super key
      terminal = "kitty";
      output = { "Virtual-1" = { mode = "1920x1440@60Hz"; }; };
      startup = [{ command = "emacs --daemon"; }];
    };
  };
}
