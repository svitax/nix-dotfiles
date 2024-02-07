{ config, lib, pkgs, ... }:
let inherit (config.colorScheme) colors;
in {
  programs.waybar = {
    enable = true;
    systemd = { enable = true; };
    settings = {
      mainBar = {
        position = "bottom";
        margin-top = 0;
        margin-bottom = 0;

        layer = "top";
        margin-left = 0;
        margin-right = 0;
        spacing = 0;

        modules-left = [ "custom/appmenu" "tray" "sway/window" ];
        modules-center = [ "sway/workspaces" "sway/mode" ];
        modules-right = [ "custom/notification" "clock#date" "clock#time" ];

        # App menu
        "custom/appmenu" = {
          format = " ";
          tooltip = false;
          on-click = "${config.wayland.windowManager.sway.config.menu}";
        };

        "sway/window" = {
          rewrite = {
            "(.*) - Brave" = "$1";
            "(.*) - Chromium" = "$1";
            "(.*) - qutebrowser" = "$1";
            "(.*) - Brave Search" = "$1";
            "(.*) - Outlook" = "$1";
            "Firefox Web Browser" = "Firefox";
            "(.*) Microsoft Teams" = "$1";
          };
          separate-outputs = true;
          max-length = 30;
        };

        "sway/mode" = { tooltip = false; };

        # System tray
        tray = {
          icon-size = 21;
          spacing = 10;
        };

        # Power Menu
        "custom/exit" = {
          format = "󰐥";
          on-click = "fuzzel-powermenu";
          tooltip = false;
        };

        # Notifications
        "custom/notification" = {
          exec = "${pkgs.swaynotificationcenter}/bin/swaync-client -swb";
          return-type = "json";
          format = "{icon}";
          format-icons = {
            notification = "󰂚";
            none = "󰂜";
            dnd-notification = "󰂛";
            dnd-none = "󰪑";
            inhibited-notification = "󰂛";
            inhibited-none = "󰪑";
            dnd-inhibited-notification = "󰂛";
            dnd-inhibited-none = "󰪑";
          };
          on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
          on-click-right =
            "${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
          tooltip = true;
          escape = true;
        };

        "clock#date" = { format = "{:%a, %b %d}"; };
        "clock#time" = { format = "{:%I:%M %p}"; };
      };
    };
    style = let
      snowflake = builtins.fetchurl rec {
        name = "Logo-${sha256}.svg";
        url =
          "https://raw.githubusercontent.com/NixOS/nixos-artwork/master/logo/nix-snowflake.svg";
        sha256 = "14mbpw8jv1w2c5wvfvj8clmjw0fi956bq5xf9s2q3my14far0as8";
      };
    in ''
      @define-color backgroundlight #${colors.base01};
      @define-color backgrounddark #${colors.base00};
      @define-color workspacesbackground1 #${colors.base00};
      @define-color workspacesbackground2 #${colors.base01};
      @define-color bordercolor #${colors.base01};
      @define-color textcolor1 #${colors.base05};
      @define-color textcolor2 #${colors.base06};
      @define-color textcolor3 #${colors.base07};
      @define-color iconcolor #${colors.base07};

      * {
        font-family: JetBrains Mono Nerd Font;
        border: none;
        border-radius: 0;
        font-size: 16px;
      }

      window#waybar {
        background: #${colors.base00};
        /* background-color: rgba(0,0,0,0.2); */
        border-bottom: 0px solid #ffffff;
        /* color: #FFFFFF; */
        transition-property: background-color;
        transition-duration: .5s;
      }

      #workspaces {
        margin-top: 8px;
        margin-left: 12px;
        margin-bottom: 8px;
        border-radius: 8px;
        transition: none;
        color: #${colors.base07};
      }

      #workspaces button {
        transition: none;
        font-size: 16px;
        border-radius: 8px;
        padding: 4px 8px;
        font-weight: bold;
        color: #${colors.base07};
      }

      #workspaces button.visible {
        transition: none;
        box-shadow: inherit;
        text-shadow: inherit;
        color: #${colors.base07};
        background: #${colors.base01};
      }

      #workspaces button:hover {
        transition: none;
        box-shadow: inherit;
        text-shadow: inherit;
        color: #${colors.base07};
        background: #${colors.base01};
      }

      tooltip {
        border-radius: 10px;
        background-color: @backgroundlight;
        padding:20px;
        margin:0px;
      }

      tooltip label {
        color: @textcolor2;
      }

      #window {
        margin-top: 8px;
        padding-left: 20px;
        padding-right: 8px;
        border-radius: 8px;
        color:@textcolor2;
        background: transparent;
        font-weight: bold;
      }

      window#waybar.empty #window {
        background-color:transparent;
      }

      .modules-left > widget:first-child > #workspaces {
        margin-left: 0;
      }

      .modules-right > widget:last-child > #workspaces {
        margin-right: 0;
      }

      #custom-system {
        margin-right: 23px;
        font-size: 20px;
        font-weight: bold;
        color: @iconcolor;
      }

      #custom-system {
        margin-right:15px;
      }

      #custom-exit {
        margin: 0px 20px 0px 0px;
        padding:0px;
        font-size:20px;
        color: @iconcolor;
      }

      #disk,#memory,#cpu,#language {
        margin:0px;
        padding:0px;
        font-size:16px;
        color:@iconcolor;
      }

      #language {
        margin-right:10px;
      }

      #clock {
        margin-top: 8px;
        margin-bottom: 0;
        transition: none;
        font-weight: bold;
        color: #${colors.base07};
      }

      #clock.date {
        margin-left: 8px;
        border-radius: 8px;
        padding-left: 16px;
        padding-right: 10px;
        border-top-right-radius: 0;
        border-bottom-right-radius: 0;
        background: #${colors.base01};
      }

      #clock.time {
        padding-left: 10px;
        padding-right: 16px;
        border-radius: 8px;
        border-top-left-radius: 0;
        border-bottom-left-radius: 0;
        margin-right: 12px;
        background: #${colors.base02};
      }

      #tray {
        background-color: #${colors.base0D};
      }

      #tray > .passive {
        -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
        -gtk-icon-effect: highlight;
        background-color: #${colors.base08};
      }

      label:focus {
        background-color: #${colors.base00};
      }

      #custom-appmenu {
        background: alpha(#ffffff, 0.05);
        border: 1px solid alpha(#ffffff, 0.1);
        border-radius: 1.5rem;
        min-width: 0.75rem;
        margin: 0.25rem 0.5rem;
      }

      #custom-appmenu {
        padding: 0.5rem;
      }

      #custom-appmenu {
        background: transparent
          url("${snowflake}")
          center/2rem no-repeat;
        border: none;
      }
    '';
  };
}
