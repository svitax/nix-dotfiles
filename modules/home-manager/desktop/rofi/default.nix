{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.desktop.rofi;
in
{
  options.desktop.rofi = {
    enable = mkEnableOption "Rofi";
  };

  config = lib.mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      font = "Iosevka Comfy 16";
      package = pkgs.rofi-wayland;
      inherit (config.window-manager.sway) terminal;
      theme = "launcher";
      extraConfig = {
        modi = "drun,run,filebrowser,window";
        case-sensitive = false;
        cycle = true;
        filter = "";
        scroll-method = 0;
        normalize-match = true;
        show-icons = true;
        steal-focus = false;
        matching = "normal";
        tokenize = true;
      };
    };
    xdg.configFile = {
      "rofi/colors.rasi".text = ''
        * {
          background: ${config.lib.stylix.colors.withHashtag.base00};
          background-alt: ${config.lib.stylix.colors.withHashtag.base00};
          foreground: ${config.lib.stylix.colors.withHashtag.base04};
          selected: ${config.lib.stylix.colors.withHashtag.base02};
          active: ${config.lib.stylix.colors.withHashtag.base0B};
          urgent: ${config.lib.stylix.colors.withHashtag.base0E};
        }
      '';
      "rofi/themes" = {
        recursive = true;
        source = ./themes;
      };
    };
  };
}
