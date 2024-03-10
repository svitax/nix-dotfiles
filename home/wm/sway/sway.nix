{ config, lib, pkgs, ... }: {
  imports = [
    ../../app/terminal/foot/foot.nix

    ./fuzzel.nix
    ./swayidle.nix
    ./swaylock.nix
    ./swayr.nix
    ./waybar.nix
    ./wob.nix
  ];
  # TODO: migrate sway.nix from hm modules to home.file
  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    shotman
    ddcutil
    pamixer
  ];
  wayland.windowManager.sway = {
    enable = true;
    # wrapperFeatures.gtk = true;
    extraConfig = ''
      default_border pixel 2
      output "*" bg /etc/wallpaper.jpg fill
    '';
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
    '';
    config = rec {
      modifier = "Mod4"; # Super key
      # terminal = "${pkgs.foot}/bin/foot";
      terminal = "${pkgs.kitty}/bin/kitty";
      menu = "${pkgs.fuzzel}/bin/fuzzel --width 50 --lines 11 --show-actions";
      gaps = { inner = 10; };
      output = { "Virtual-1" = { mode = "1920x1440@60Hz"; }; };
      bars = [ ];
      startup = [
        { command = "exec sleep 5; systemctl --user start wob.service"; }
        { command = "exec sleep 5; systemctl --user start swayrd.service"; }
        { command = "exec sleep 5; systemctl --user start waybar.service"; }
        { command = "exec sleep 5; systemctl --user start emacs.service"; }
      ];
      keybindings = let
        cfg = config.wayland.windowManager.sway.config;
        mod = cfg.modifier;
        backup_terminal = "emacsclient -n -c -e '(eshell \"new\")'";
        # mod+o = # TODO: focus next
        browser = "${pkgs.qutebrowser}/bin/qutebrowser";
        pamixer = "${pkgs.pamixer}/bin/pamixer";
        # mod+shift+n = # TODO: swaync client
      in lib.mkOptionDefault {
        # TODO: set up eshell keybind
        "${mod}+Return" = "exec ${cfg.terminal}";
        "${mod}+Control+Return" = "exec ${backup_terminal}";
        "${mod}+c" = "kill";
        "${mod}+d" = "exec ${cfg.menu}";
        "${mod}+Shift+x" = "exec swaylock -c 000000";
        "${mod}+i" = "exec ${browser}";
        # "${mod}+Shift+q" = "exec ${cfg.power-menu}";
        # "${mod}+p" = "exec ${cfg.pass}";
        # TODO: set up EDITOR to use emacsclient so I can just exec that
        "${mod}+n" = "exec emacsclient -cn";
        # TODO: set up mail keybind
        # "${mod}+m" = "mail";

        "${mod}+${cfg.left}" = "focus left";
        "${mod}+${cfg.down}" = "focus down";
        "${mod}+${cfg.up}" = "focus up";
        "${mod}+${cfg.right}" = "focus right";

        "${mod}+Shift+${cfg.left}" = "move left";
        "${mod}+Shift+${cfg.down}" = "move down";
        "${mod}+Shift+${cfg.up}" = "move up";
        "${mod}+Shift+${cfg.right}" = "move right";

        "${mod}+b" = "splitv";
        "${mod}+v" = "splith";
        "${mod}+f" = "fullscreen toggle";
        "${mod}+a" = "focus parent";

        "${mod}+s" = "layout stacking";
        "${mod}+w" = "layout tabbed";
        "${mod}+e" = "layout toggle split";

        "${mod}+Shift+space" = "floating toggle";
        # toggle between tiling/floating windows
        "${mod}+Ctrl+space" = "focus mode_toggle";

        "${mod}+1" = "workspace number 1";
        "${mod}+2" = "workspace number 2";
        "${mod}+3" = "workspace number 3";
        "${mod}+4" = "workspace number 4";
        "${mod}+5" = "workspace number 5";
        "${mod}+6" = "workspace number 6";
        "${mod}+7" = "workspace number 7";
        "${mod}+8" = "workspace number 8";
        "${mod}+9" = "workspace number 9";
        "${mod}+Tab" = "exec swayr switch-window";
        "${mod}+Shift+Tab" = "exec swayr switch-to-urgent-or-lru-window";
        "${mod}+o" = "exec swayr next-window current-workspace";

        "${mod}+Shift+1" = "move container to workspace number 1";
        "${mod}+Shift+2" = "move container to workspace number 2";
        "${mod}+Shift+3" = "move container to workspace number 3";
        "${mod}+Shift+4" = "move container to workspace number 4";
        "${mod}+Shift+5" = "move container to workspace number 5";
        "${mod}+Shift+6" = "move container to workspace number 6";
        "${mod}+Shift+7" = "move container to workspace number 7";
        "${mod}+Shift+8" = "move container to workspace number 8";
        "${mod}+Shift+9" = "move container to workspace number 9";

        "${mod}+Shift+minus" = "move scratchpad";
        "${mod}+minus" = "scratchpad show";

        "${mod}+Shift+r" = "reload";
        "${mod}+Shift+e" =
          "exec swaynag -t warning -m 'Do you really want to exit?' -b 'Yes' 'swaymsg exit'";

        "${mod}+r" = "mode resize";

        # TODO: Make brightness and volume keys work
        # Volume key bindings
        "XF86AudioRaiseVolume" =
          "exec ${pamixer} -ui 2 && ${pamixer} --get-volume > $XDG_RUNTIME_DIR/wob.sock";
        "XF86AudioLowerVolume" =
          "exec ${pamixer} -ud 2 && ${pamixer} --get-volume > $XDG_RUNTIME_DIR/wob.sock";
        "XF86AudioMute" =
          "exec ${pamixer} -t && ${pamixer} --get-volume > $XDG_RUNTIME_DIR/wob.sock";

        "${mod}+Print" = "exec shotman -c output";
        "${mod}+Shift+Print" = "exec shotman -c region";
        "${mod}+Alt+Print" = "exec shotman -c window";
      };
      fonts = {
        names = [ "JetBrainsMono Nerd Font" ];
        size = 15.0;
      };
      colors = {
        background = "#ffffff";
        urgent = {
          background = "#${config.colorScheme.colors.base08}";
          border = "#${config.colorScheme.colors.base08}";
          childBorder = "#${config.colorScheme.colors.base08}";
          indicator = "#${config.colorScheme.colors.base08}";
          text = "#${config.colorScheme.colors.base07}";
        };
        focused = {
          background = "#${config.colorScheme.colors.base02}";
          border = "#${config.colorScheme.colors.base02}";
          childBorder = "#${config.colorScheme.colors.base02}";
          indicator = "#${config.colorScheme.colors.base02}";
          text = "#${config.colorScheme.colors.base07}";
        };
        focusedInactive = {
          background = "#${config.colorScheme.colors.base01}";
          border = "#${config.colorScheme.colors.base01}";
          childBorder = "#${config.colorScheme.colors.base01}";
          indicator = "#${config.colorScheme.colors.base01}";
          text = "#${config.colorScheme.colors.base04}";
        };
        unfocused = {
          background = "#${config.colorScheme.colors.base01}";
          border = "#${config.colorScheme.colors.base01}";
          childBorder = "#${config.colorScheme.colors.base01}";
          indicator = "#${config.colorScheme.colors.base01}";
          text = "#${config.colorScheme.colors.base04}";
        };
      };
    };
  };
}
