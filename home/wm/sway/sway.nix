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
    mpc-cli
    brightnessctl
    autotiling
    sway-overfocus
  ];
  wayland.windowManager.sway = {
    enable = true;
    # xwayland = true;
    wrapperFeatures.gtk = true;
    extraConfig = ''
      exec_always autotiling
      titlebar_border_thickness 3
      titlebar_padding 4 3
      title_align left
      default_border pixel 4
      hide_edge_borders smart

      focus_follows_mouse no
      mouse_warping output
      focus_wrapping force

      output "*" bg /etc/wallpaper.jpg fill
    '';
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export MOZ_DBUS_REMOTE=1
      export MOZ_WEBRENDER=1
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_DESKTOP=sway
    '';
    config = {
      modifier = "Mod4"; # Super key
      terminal = "${pkgs.foot}/bin/foot";
      menu = "${pkgs.fuzzel}/bin/fuzzel --width 50 --lines 11 --show-actions";
      gaps = { inner = 10; };
      output = { "Virtual-1" = { mode = "1920x1440@60Hz"; }; };
      bars = [ ];
      startup = [
        { command = "swaymsg workspace 1"; }
        { command = "exec sleep 2; autotiling"; }
        { command = "exec sleep 2; systemctl --user start wob.service"; }
        { command = "exec sleep 2; systemctl --user start swayrd.service"; }
        { command = "exec sleep 2; systemctl --user start waybar.service"; }
        { command = "exec sleep 2; systemctl --user start emacs.service"; }
      ];
      keybindings = let
        cfg = config.wayland.windowManager.sway.config;
        mod = cfg.modifier;
        # backup_terminal = "emacsclient -n -c -e '(eshell \"new\")'";
        backup_terminal = "footclient";
        browser = "${pkgs.qutebrowser}/bin/qutebrowser";
        mpc = "${pkgs.mpc-cli}/bin/mpc";
        pamixer = "${pkgs.pamixer}/bin/pamixer";
        sed = "${pkgs.gnused}/bin/sed";
        brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
        # mod+shift+n = # TODO: swaync client
      in lib.mkOptionDefault {
        # TODO: set up eshell keybind
        "${mod}+Return" = "exec ${cfg.terminal}";
        "${mod}+Control+Return" = "exec ${backup_terminal}";
        "${mod}+c" = "kill";
        "${mod}+d" = "exec ${cfg.menu}";
        "${mod}+i" = "exec ${browser}";
        "${mod}+o" = "exec swayr next-window current-workspace";
        # "${mod}+m" = "mail";
        "${mod}+n" = "exec $EDITOR";
        # "${mod}+p" = "exec $PASS";
        "${mod}+Tab" = "exec sway-overfocus group-rw group-dw";
        "${mod}+Shift+Tab" = "exec sway-overfocus group-lw group-uw";
        # "${mod}+Shift+q" = "exec ${cfg.power-menu}";
        "${mod}+Shift+x" = "exec swaylock -c 000000";
        # TODO: set up mail keybind

        # "${mod}+${cfg.left}" = "focus left";
        # "${mod}+${cfg.down}" = "focus down";
        # "${mod}+${cfg.up}" = "focus up";
        # "${mod}+${cfg.right}" = "focus right";

        "${mod}+${cfg.left}" =
          "exec sway-overfocus split-lt float-lt output-ls";
        "${mod}+${cfg.down}" =
          "exec sway-overfocus split-dt float-dt output-ds";
        "${mod}+${cfg.up}" = "exec sway-overfocus split-ut float-ut output-us";
        "${mod}+${cfg.right}" =
          "exec sway-overfocus split-rt float-rt output-rs";

        "${mod}+Shift+${cfg.left}" = "move left";
        "${mod}+Shift+${cfg.down}" = "move down";
        "${mod}+Shift+${cfg.up}" = "move up";
        "${mod}+Shift+${cfg.right}" = "move right";

        "${mod}+b" = "splitv";
        "${mod}+v" = "splith";
        "${mod}+f" = "fullscreen toggle";
        "${mod}+a" = "focus parent";

        # subdivides the containers, alternating between vertical and horizontal split
        "${mod}+s" = "split toggle";
        # subdivides the containers into tabs
        "${mod}+w" = "split toggle; layout tabbed";
        # change container layout (stacked, tabbed, toggle split)
        "${mod}+t" = "layout toggle splith splitv tabbed";
        # collapse singleton parent(s)
        "${mod}+e" = "split none";

        # "${mod}+s" = "layout stacking";
        # "${mod}+w" = "layout tabbed";
        # "${mod}+e" = "layout toggle split";

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

        "XF86AudioStop" = "exec ${mpc} stop";
        "XF86AudioPlay" = "exec ${mpc} toggle";
        "XF86AudioPause" = "exec ${mpc} toggle";
        "XF86AudioNext" = "exec ${mpc} next";
        "XF86AudioPrev" = "exec ${mpc} prev";

        "${mod}+XF86AudioMute" = "exec ${pamixer} --default-source -t";
        "${mod}+m" = "exec ${pamixer} --default-source -t";

        "Shift+XF86AudioRaiseVolume" =
          "exec ${mpc} vol +2 && ${mpc} vol | ${sed} 's|n/a|0%|g;s/[^0-9]*//g' > $XDG_RUNTIME_DIR/wob.sock";
        "Shift+XF86AudioLowerVolume" =
          "exec ${mpc} vol -2 && ${mpc} vol | ${sed} 's|n/a|0%|g;s/[^0-9]*//g' > $XDG_RUNTIME_DIR/wob.sock";

        "XF86MonBrightnessDown" =
          "exec ${brightnessctl} set 5%- | ${sed} -En 's/.*\\(([0-9]+)%\\).*/\\1/p' > $XDG_RUNTIME_DIR/wob.sock";
        "XF86MonBrightnessUp" =
          "exec ${brightnessctl} set 5%+ | ${sed} -En 's/.*\\(([0-9]+)%\\).*/\\1/p' > $XDG_RUNTIME_DIR/wob.sock";

        "${mod}+Print" = "exec shotman -c output";
        "${mod}+Shift+Print" = "exec shotman -c region";
        "${mod}+Alt+Print" = "exec shotman -c window";
      };
      fonts = {
        names = [ "JetBrainsMono Nerd Font" ];
        size = 16.0;
      };
      colors = {
        background = "#ffffff";
        focused = {
          border = "#${config.colorScheme.colors.base03}";
          background = "#${config.colorScheme.colors.base02}";
          text = "#${config.colorScheme.colors.base06}";
          indicator = "#${config.colorScheme.colors.base02}";
          childBorder = "#${config.colorScheme.colors.base04}";
        };
        focusedInactive = {
          border = "#${config.colorScheme.colors.base03}";
          background = "#${config.colorScheme.colors.base02}";
          text = "#${config.colorScheme.colors.base06}";
          indicator = "#${config.colorScheme.colors.base02}";
          childBorder = "#${config.colorScheme.colors.base01}";
        };
        unfocused = {
          border = "#${config.colorScheme.colors.base03}";
          background = "#${config.colorScheme.colors.base01}";
          text = "#${config.colorScheme.colors.base04}";
          indicator = "#${config.colorScheme.colors.base01}";
          childBorder = "#${config.colorScheme.colors.base00}";
        };
        urgent = {
          border = "#${config.colorScheme.colors.base08}";
          background = "#${config.colorScheme.colors.base08}";
          text = "#${config.colorScheme.colors.base07}";
          indicator = "#${config.colorScheme.colors.base08}";
          childBorder = "#${config.colorScheme.colors.base08}";
        };
      };
    };
  };
}
