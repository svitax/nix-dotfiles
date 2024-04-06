{ config, lib, pkgs, ... }:
let inherit (config.colorScheme) colors;
in {
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [ thumbnail ];
    config = {
      # audio
      # ao = "pipewire";
      # ao = "pulse";
      # af = "rubberband";
      osc = "no";

      # cache-secs = "60";
      # sub-auto = "fuzzy";
      # sid = "auto";
      ytdl-format = "bestvideo[height<=?1440]+bestaudio/best";
      # ytdl-raw-options =
      #   "write-sub=,write-auto-sub=,sub-lang=en,sub-format=en,write-srt=";
      script-opts = "ytdl_hook-ytdl_path=${pkgs.yt-dlp}/bin/yt-dlp";

      hwdec = "auto-safe";
      # hwdec-codecs = "all";
      vo = "gpu";
      profile = "gpu-hq";

      title = "\${?media-title:\${media-title}}\${!media-title:No file.}";
      # video-sync = "display-resample";

      # slang = "en";
      gpu-context = "wayland";
    };
    bindings = {
      p = "cycle pause";
      h = "seek -5";
      l = "seek 5";
      H = "seek 60";
      L = "seek -60";
      j = "add volume -3";
      k = "add volume 3";
    };
  };
}
