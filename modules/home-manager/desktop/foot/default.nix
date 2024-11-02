{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.desktop.foot;
in
{
  options.desktop.foot = {
    enable = mkEnableOption "Foot terminal emulator";
  };

  config = lib.mkIf cfg.enable {
    programs.foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          font = "Iosevka Comfy:size=16";
          pad = "10x10";
        };
        mouse = {
          hide-when-typing = "yes";
        };
        colors = {
          foreground = "${config.lib.stylix.colors.base05}";
          background = "${config.lib.stylix.colors.base00}";
          regular0 = "${config.lib.stylix.colors.base00}";
          regular1 = "${config.lib.stylix.colors.base08}";
          regular2 = "${config.lib.stylix.colors.base0B}";
          regular3 = "${config.lib.stylix.colors.base0A}";
          regular4 = "${config.lib.stylix.colors.base0D}";
          regular5 = "${config.lib.stylix.colors.base0E}";
          regular6 = "${config.lib.stylix.colors.base0C}";
          regular7 = "${config.lib.stylix.colors.base05}";
          bright0 = "${config.lib.stylix.colors.base03}";
          bright1 = "${config.lib.stylix.colors.base08}";
          bright2 = "${config.lib.stylix.colors.base0B}";
          bright3 = "${config.lib.stylix.colors.base0A}";
          bright4 = "${config.lib.stylix.colors.base0D}";
          bright5 = "${config.lib.stylix.colors.base0E}";
          bright6 = "${config.lib.stylix.colors.base0C}";
          bright7 = "${config.lib.stylix.colors.base07}";
          "16" = "${config.lib.stylix.colors.base09}";
          "17" = "${config.lib.stylix.colors.base0F}";
          "18" = "${config.lib.stylix.colors.base01}";
          "19" = "${config.lib.stylix.colors.base02}";
          "20" = "${config.lib.stylix.colors.base04}";
          "21" = "${config.lib.stylix.colors.base06}";
        };
      };
    };
  };
}
