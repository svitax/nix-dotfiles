{ config, lib, pkgs, ... }:
let inherit (config.colorScheme) colors;
in {
  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        event = "lock";
        command = "${pkgs.swaylock}/bin/swaylock";
      }
    ];
    timeouts = [{
      timeout = 1800;
      command = "${pkgs.systemd}/bin/loginctl lock-session";
    }];
  };
}
