{
  pkgs,
  inputs,
  ...
}: {
  home.packages = with pkgs; [firefox-gnome-theme];
  programs.firefox = {
    enable = true;
    profiles.svitax = {
      settings = {
        "dom.security.https_only_mode" = true;
        "browser.download.panel.shown" = true;
        "identity.fxaccounts.enabled" = false;
        "sigon.rememberSignons" = false;
      };
      extensions = [
        inputs.firefox-addons.packages."x86_64-linux".bitwarden
        inputs.firefox-addons.packages."x86_64-linux".ublock-origin
        inputs.firefox-addons.packages."x86_64-linux".sponsorblock
        inputs.firefox-addons.packages."x86_64-linux".darkreader
        inputs.firefox-addons.packages."x86_64-linux".tridactyl
        inputs.firefox-addons.packages."x86_64-linux".youtube-shorts-block
      ];
      search.engines = {
        "Nix Packages" = {
          urls = [
            {
              template = "https://search.nixos.org/packages";
              params = [
                {
                  name = "type";
                  value = "packages";
                }
                {
                  name = "query";
                  value = "{searchTerms}";
                }
              ];
            }
          ];
          icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
          definedAliases = ["@np"];
        };
      };
      search.force = true;
      bookmarks = [
        {
          name = "wikipedia";
          tags = ["wiki"];
          keyword = "wiki";
          url = "https://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go";
        }
      ];
      userChrome = ''
        /* some css */
      '';
    };
  };
}
