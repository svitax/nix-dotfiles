{ config, lib, pkgs, ... }:
let inherit (config.colorScheme) colors;
in {
  home.packages = with pkgs; [ wob pulseaudio ];
  systemd.user.services.wob = {
    Unit = {
      Description =
        "A lightweight overlay volume/backlight/progress/anything bar for Wayland";
      Documentation = "man:wob(1)";
      PartOf = "graphical-session.target";
      After = "graphical-session.target";
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    Service = {
      StandardInput = "socket";
      ExecStart = "${pkgs.wob}/bin/wob";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.sockets.wob = {
    Socket = {
      ListenFIFO = "%t/wob.sock";
      SocketMode = "0600";
      RemoveOnStop = "yes";
      FlushPending = "yes";
    };
    Install.WantedBy = [ "sockets.target" ];
  };

  xdg.configFile."wob/wob.ini".text = ''
    border_offset=0
    border_size=2
    bar_padding=8
    border_color=${colors.base02}ff
    background_color=${colors.base00}ff
    bar_color=${colors.base03}ff
  '';
}
