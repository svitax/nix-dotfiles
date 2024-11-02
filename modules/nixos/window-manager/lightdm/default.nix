{
  lib,
  config,
  ...
}:
with lib;
let
  cfg = config.window-manager.lightdm;
in
{
  options.window-manager.lightdm = {
    enable = mkEnableOption "Enables the Emacs X Window Manager";
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager = {
      # autoLogin.enable = true;
      # autoLogin.user = "evermind";
      # defaultSession = "none+exwm";
      lightdm = {
        enable = true;
        greeters.mini = {
          enable = true;
          extraConfig = ''
              [greeter]
            user = evermind
            password-alignment = left
            password-label-text = ""
            password-input-width = 30
            show-password-label = false

            [greeter-theme]
            font-size = 10px
            font = "Iosevka"
            password-background-color = "#1e1e1e"
            background-color = "#000000"
            text-color = "#ffffff"
            window-color = "#000000"
            border-color = "#feacd0"
            background-image = ""
          '';
        };
      };
    };
  };
}
