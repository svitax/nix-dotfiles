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
        format = "(bestvideo[vcodec^=av01][height<=?1440]/bestvideo[height<=?1440])+bestaudio/best";
        audio-format = "best";
        embed-subs = true;
        embed-thumbnail = true;
        embed-chapters = true;
        embed-info-json = true;
        embed-metadata = true;
        write-subs = true;
        write-auto-subs = true;
      };
    };
  };
}
