# "custom/power" = {
#   format = "󰐥";
#   on-click = "${pkgs.systemd}/bin/systemctl poweroff";
#   tooltip = false;
# };
# "custom/exit" = {
#   format = "󰈆";
#   on-click = "${pkgs.systemd}/bin/loginctl terminate-user $USER";
#   tooltip = false;
# };
# "custom/lock" = {
#   format = "󰌾";
#   on-click = "${pkgs.systemd}/bin/loginctl lock-session";
#   tooltip = false;
# };
# "custom/suspend" = {
#   format = "󰤄";
#   on-click = "${pkgs.systemd}/bin/systemctl suspend";
#   tooltip = false;
# };
# "custom/reboot" = {
#   format = "󰜉";
#   on-click = "${pkgs.systemd}/bin/systemctl reboot";
#   tooltip = false;
# };
{ }
