{
  config,
  lib,
  pkgs,
  ...
}:
#
with lib; let
  cfg = config.window-manager.sway;
in {
  options.window-manager.sway = {
    enable = mkEnableOption "Sway window manager";

    terminal = mkOption {
      type = types.str;
      default = "${config.programs.foot.package}/bin/footclient";
      description = "Terminal to use";
    };

    menu = mkOption {
      type = types.str;
      default = "${config.programs.rofi.package}/bin/rofi";
      description = "Menu to use";
    };
  };

  config = mkIf cfg.enable {
    wayland.windowManager.sway = {
      enable = true;
      config = {
        modifier = "Mod4";
        terminal = cfg.terminal;
        menu = cfg.menu;
        gaps = {
          inner = 10;
          outer = 20;
        };
        output = {"Virtual-1" = {mode = "1920x1080@60Hz";};};
        defaultWorkspace = "workspace number 1";
        keybindings = let
          mod = config.wayland.windowManager.sway.config.modifier;
          left = "m";
          down = "n";
          up = "e";
          right = "i";
        in {
          "${mod}+Return" = "exec ${cfg.terminal}";
          "${mod}+d" = "exec ${cfg.menu} -show drun";
          "${mod}+p" = "exec $EDITOR";
          "${mod}+k" = "kill";
          "${mod}+b" = "exec ${cfg.menu} -show window";

          "${mod}+${left}" = "focus left";
          "${mod}+${down}" = "focus down";
          "${mod}+${up}" = "focus up";
          "${mod}+${right}" = "focus right";

          "${mod}+Shift+${left}" = "move left";
          "${mod}+Shift+${down}" = "move down";
          "${mod}+Shift+${up}" = "move up";
          "${mod}+Shift+${right}" = "move right";

          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 0";
        };
        fonts = {
          names = ["Iosevka Comfy"];
          size = 18.0;
        };
        colors = {
          focused = {
            border = "#545454";
            background = "#cab9b2";
            text = "${config.lib.stylix.colors.withHashtag.base04}";
            indicator = "#545454";
            childBorder = "#545454";
          };
          # focusedInactive = {
          #   border = "${}";
          #   background = "${}";
          #   text = "${}";
          #   indicator = "${}";
          #   childBorder = "${}";
          # };
          unfocused = {
            border = "#a59a94";
            background = "${config.lib.stylix.colors.withHashtag.base07}";
            text = "${config.lib.stylix.colors.withHashtag.base06}";
            indicator = "${config.lib.stylix.colors.withHashtag.base07}";
            childBorder = "#a59a94";
          };
          # urgent = {
          #   border = "${}";
          #   background = "${}";
          #   text = "${}";
          #   indicator = "${}";
          #   childBorder = "${}";
          # };
        };
      };
      extraConfig = ''
        title_align center
        titlebar_border_thickness 1
        default_border normal 1
        focus_follows_mouse no
      '';
    };

    # TODO: extract gtk config
    # gtk = {
    #   enable = true;

    #   cursorTheme = {
    #     package = pkgs.bibata-cursors;
    #     name = "Bibata-Modern-Ice";
    #   };

    #   theme = {
    #     package = pkgs.adw-gtk3;
    #     name = "adw-gtk3";
    #   };
    # };

    # TODO: extract cursor config
    # home.pointerCursor = {
    #   name = "Bibata-Modern-Ice";
    #   package = pkgs.bibata-cursors;
    #   gtk.enable = true;
    #   x11 = {
    #     enable = true;
    #     defaultCursor = "Bibata-Modern-Ice";
    #   };
    # };

    # TODO: extract qt config
    # qt = {
    #   enable = true;
    #   platformTheme.name = "gtk";
    #   style.name = "adwaita-dark";
    #   style.package = pkgs.adwaita-qt;
    # };

    home.sessionVariables = {
      MOZ_DBUS_REMOTE = "1";
      MOZ_WEBRENDER = "1";
      # Configure Firefox to use Wayland
      MOZ_ENABLE_WAYLAND = "1";
      # Set XDG portal related variables
      XDG_CURRENT_DESKTOP = "sway";
      XDG_SESSION_DESKTOP = "sway";
      XDG_SESSION_TYPE = "wayland";
      # If your cursor becomes invisible
      WLR_NO_HARDWARE_CURSORS = "1";
      # Hint electron apps to use wayland
      NIXOS_OZONE_WL = "1";
    };
  };
}
