{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.desktop.media.yt-dlp;
in
{
  options.modules.desktop.media.yt-dlp = {
    enable = mkEnableOption "";
  };

  # TODO extract out any conf files for yt-dlp so i can easily migrate away from
  # nix
  config = lib.mkIf cfg.enable {
    programs.yt-dlp = {
      enable = true;
      settings = {
        embed-thumbnail = true;
        embed-metadata = true;
        embed-subs = true;
        sub-langs = "en";
      };
    };
  };
}
