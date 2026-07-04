{
  ...
}:
{
  flake.modules.homeManager.librewolf =
    { pkgs, config, ... }:
    let
      profileName = config.home.username;
    in
    {
      programs.firefox = {
        enable = true;
        package = pkgs.librewolf;
        policies = {
          DontCheckDefaultBrowser = true;
          DisablePocket = true;
          DisableAppUpdate = true;
          WebsiteFilter = {
            Block = [
              "*://*.youtube.com/*"
              "*://youtube.com/*"
            ];
          };
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
              (extension "istilldontcareaboutcookies" "idcac-pub@guus.ninja")
            ];
        };
      };

      home.file.".librewolf/profiles.ini".text = ''
        [Profile0]
        Name=default
        IsRelative=1
        Path=${profileName}.default
        Default=1

        [General]
        StartWithLastProfile=1
        Version=2
      '';

      home.file.".librewolf/${profileName}.default/user.js".source = ./user.js;
    };
}
