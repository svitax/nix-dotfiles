{ config, lib, pkgs, ... }:
let inherit (config.colorScheme) colors;
in {
  programs.swayr = {
    enable = true;
    systemd = { enable = true; };
    settings = {
      menu = {
        executable = "fuzzel";
        args = [
          "--dmenu"
          "--width=50"
          "--lines=11"
          "--font=JetBrains Mono Nerd Font:size=15"
          # "--line-height=28"
          # "--icon-theme=${config.gtk.iconTheme.name}"
        ];
      };
    };
    extraConfig = ''
      [format]
      window_format = "{app_name:{:<10.10}} {title:{:<15.15}...} on {workspace_name:{:<10.10}} \u0000icon\u001f{app_icon}"
      indent = '  '
      icon_dirs = [
                '/run/current-system/sw/share/icons/hicolor/scalable/apps/',
                '/run/current-system/sw/share/icons/hicolor/48x48/apps/',
                '/run/current-system/sw/share/pixmaps',
                '~/.nix-profile/share/icons/hicolor/scalable/apps/',
                '~/.nix-profile/share/icons/hicolor/48x48/apps/',
                '~/.nix-profile/share/pixmaps',
                '/usr/share/icons/hicolor/scalable/apps',
                '/usr/share/icons/hicolor/64x64/apps',
                '/usr/share/icons/hicolor/48x48/apps',
                '/usr/share/icons/Adwaita/64x64/apps',
                '/usr/share/icons/Adwaita/48x48/apps',
                '/usr/share/pixmaps',
               ]
    '';
  };
}
