{ pkgs, lib, config, ... }:
let inherit (config.colorScheme) colors;
in {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "JetBrains Mono Nerd Font:size=16";
        # dpi-aware = "yes";
        term = "foot";
        pad = "10x10";
        # shell = "${pkgs.fish}/bin/fish";
        # login-shell = "no";
      };
      mouse = { hide-when-typing = "yes"; };
      colors = {
        foreground = "${colors.base05}";
        background = "${colors.base00}";
        regular0 = "${colors.base00}";
        regular1 = "${colors.base08}";
        regular2 = "${colors.base0B}";
        regular3 = "${colors.base0A}";
        regular4 = "${colors.base0D}";
        regular5 = "${colors.base0E}";
        regular6 = "${colors.base0C}";
        regular7 = "${colors.base05}";
        bright0 = "${colors.base03}";
        bright1 = "${colors.base08}";
        bright2 = "${colors.base0B}";
        bright3 = "${colors.base0A}";
        bright4 = "${colors.base0D}";
        bright5 = "${colors.base0E}";
        bright6 = "${colors.base0C}";
        bright7 = "${colors.base07}";
        "16" = "${colors.base09}";
        "17" = "${colors.base0F}";
        "18" = "${colors.base01}";
        "19" = "${colors.base02}";
        "20" = "${colors.base04}";
        "21" = "${colors.base06}";
      };
    };
  };
}
