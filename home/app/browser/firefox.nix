{ pkgs, inputs, ... }:
let addons = inputs.firefox-addons.packages.${pkgs.system};
in {
  home.file.".mozilla/firefox/svitax/chrome/firefox-gnome-theme".source =
    inputs.firefox-gnome-theme;
  programs.firefox = {
    enable = true;
    profiles.svitax = {
      id = 0;
      isDefault = true;
      settings = {
        "dom.security.https_only_mode" = true;
        "browser.download.panel.shown" = true;
        "identity.fxaccounts.enabled" = false;
        "sigon.rememberSignons" = false;
        "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
        # Firefox gnome theme ## - https://github.com/rafaelmardojai/firefox-gnome-theme/blob/v120/configuration/user.js
        # (copied into here becase home-manager already writes to user.js)
        "toolkit.legacyUserProfileCustomizations.stylesheets" =
          true; # Enable customChrome.css
        "browser.uidensity" = 0; # Set UI density to normal
        "svg.context-properties.content.enabled" =
          true; # Enable SVG context-properties
        "browser.theme.dark-private-windows" =
          false; # Disable private window dark theme
        "widget.gtk.rounded-bottom-corners.enabled" =
          true; # Enable rounded bottom window corners
      };
      extensions = with addons; [
        bitwarden
        ublock-origin
        sponsorblock
        darkreader
        youtube-shorts-block
        vimium-c
      ];
      userChrome =
        # css
        ''
          @import "firefox-gnome-theme/userChrome.css";
        '';
      userContent =
        # css
        ''
          @import "firefox-gnome-theme/userContent.css"
        '';
      search = {
        default = "Google";
        force = true;
        engines = {
          "Nix Packages" = {
            urls = [{
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
            }];
            icon =
              "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
            definedAliases = [ "@np" ];
          };
        };
      };
      bookmarks = [{
        name = "wikipedia";
        tags = [ "wiki" ];
        keyword = "wiki";
        url = "https://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go";
      }];
    };
  };
}
