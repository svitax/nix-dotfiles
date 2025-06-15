{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.browser.qutebrowser;
in
{
  options.browser.qutebrowser = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      qutebrowser
    ];

    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "text/html" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
      };
    };

    home.file.".config/qutebrowser/config.py".source = ./config.py;
    home.file.".config/qutebrowser/all-sites.css".source = ./all-sites.css;
    # Make sure to do `:greasemonkey-reload' every time you change or add a
    # script.
    home.file.".config/qutebrowser/greasemonkey" = {
      source = ./greasemonkey;
      recursive = true;
    };
  };
}
