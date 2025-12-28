{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption mkOption;

  cfg = config.modules.desktop.browsers.librewolf;
in
{
  options.modules.desktop.browsers.librewolf = {
    enable = mkEnableOption "";
    profileName = mkOption {
      type = lib.types.str;
      default = config.home.username;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package = pkgs.librewolf;
      policies = {
        DontCheckDefaultBrowser = true;
        DisablePocket = true;
        DisableAppUpdate = true;
        ExtensionSettings =
          with builtins;
          let
            extension = shortId: uuid: {
              name = uuid;
              value = {
                install_url = "https://addons.mozilla.org/en-US/firefox/downloads/latest/${shortId}/latest.xpi";
                installation_mode = "normal_installed";
              };
            };
          in
          listToAttrs [
            (extension "bitwarden-password-manager" "{446900e4-71c2-419f-a6a7-df9c091e268b}")
          ];
      };
    };

    home.file.".librewolf/profiles.ini".text = ''
      [Profile0]
      Name=default
      IsRelative=1
      Path=${cfg.profileName}.default
      Default=1

      [General]
      StartWithLastProfile=1
      Version=2
    '';

    home.file.".librewolf/${cfg.profileName}.default/user.js" = {
      source = ./user.js;
    };
  };
}
