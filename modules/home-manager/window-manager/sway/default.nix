{
  config,
  lib,
  ...
}:
#
with lib;
let
  cfg = config.window-manager.sway;
in
{
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
        inherit (cfg) terminal;
        inherit (cfg) menu;
        gaps = {
          bottom = 0;
          horizontal = 0;
          left = 0;
          outer = 0;
          right = 0;
          top = 0;
          inner = 0;
          vertical = 0;
        };
        output = {
          "Virtual-1" = {
            mode = "1920x1080@60Hz";
          };
        };
        defaultWorkspace = "workspace number 1";
        keybindings =
          let
            mod = config.wayland.windowManager.sway.config.modifier;
            left = "y";
            down = "h";
            up = "a";
            right = "e";
          in
          {
            "${mod}+Return" = "exec $EDITOR";
            "${mod}+b" = "exec ${cfg.menu} -show window";
            "${mod}+d" = "exec ${cfg.menu} -show drun";
            "${mod}+k" = "kill";
            # "${mod}+t" = "exec ${cfg.terminal}";

            "${mod}+${left}" = "focus left";
            "${mod}+${down}" = "focus down";
            "${mod}+${up}" = "focus up";
            "${mod}+${right}" = "focus right";

            "${mod}+Shift+${left}" = "move left";
            "${mod}+Shift+${down}" = "move down";
            "${mod}+Shift+${up}" = "move up";
            "${mod}+Shift+${right}" = "move right";

            "${mod}+s" = "layout toggle stacking split";
            "${mod}+t" = "layout toggle tabbed split";
            "${mod}+f" = "fullscreen";

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
          names = [ "Iosevka Comfy" ];
          size = 14.0;
        };
        colors = {
          focused = {
            border = "#5a5a5a";
            background = "#bfc9ff";
            text = "${config.lib.stylix.colors.withHashtag.base04}";
            indicator = "#bfc9ff";
            childBorder = "#5a5a5a";
          };
          focusedInactive = {
            border = "#5a5a5a";
            background = "${config.lib.stylix.colors.withHashtag.base07}";
            text = "${config.lib.stylix.colors.withHashtag.base06}";
            indicator = "${config.lib.stylix.colors.withHashtag.base07}";
            childBorder = "${config.lib.stylix.colors.withHashtag.base07}";
          };
          unfocused = {
            border = "#5a5a5a";
            background = "${config.lib.stylix.colors.withHashtag.base07}";
            text = "${config.lib.stylix.colors.withHashtag.base06}";
            indicator = "${config.lib.stylix.colors.withHashtag.base07}";
            childBorder = "${config.lib.stylix.colors.withHashtag.base07}";
          };
          urgent = {
            border = "${config.lib.stylix.colors.withHashtag.base08}";
            background = "${config.lib.stylix.colors.withHashtag.base08}";
            text = "${config.lib.stylix.colors.withHashtag.base06}";
            indicator = "${config.lib.stylix.colors.withHashtag.base08}";
            childBorder = "${config.lib.stylix.colors.withHashtag.base08}";
          };
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
