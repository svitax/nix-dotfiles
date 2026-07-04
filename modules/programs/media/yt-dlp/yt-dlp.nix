{
  ...
}:
{
  flake.modules.homeManager.yt-dlp = {
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
