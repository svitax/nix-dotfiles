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
