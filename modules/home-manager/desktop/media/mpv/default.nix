{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.desktop.media.mpv;
in
{
  options.modules.desktop.media.mpv = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    modules.desktop.media.yt-dlp.enable = true;

    programs.mpv = {
      enable = true;
      package = pkgs.mpv.override {
        scripts = with pkgs.mpvScripts; [
          uosc
          sponsorblock-minimal
          thumbfast
          quality-menu
          mpv-playlistmanager
          reload
          autoload
          # autocrop
        ];
      };
      # TODO extract out the .conf files for my mpv config so it is easier to
      # migrate later
      config = {
        volume = 60;
        volume-max = 100;
        save-watch-history = true;
        write-filename-in-watch-later-config = true;
        watch-later-directory = "${config.xdg.stateHome}/mpv/watch_later";
        keep-open = "always";
        geometry = "50%x100%"; # only relevant for non-tiling WMs
        hwdec = "auto-safe";
        vo = "gpu-next";
        ytdl-format = "(bestvideo[vcodec^=av01][height<=?2160]/bestvideo[height<=?2160])+bestaudio/best";
      };
      scriptOpts = {
        autoload = {
          disabled = false;
          images = false;
          videos = true;
          audio = true;
          ignore_hidden = true;
        };
        uosc = {
          timeline_size = 25;
          timeline_persistency = "paused,idle,audio";
          #   progress = "always";
          progress_size = 4;
          progress_line_width = 4;
          controls = "menu,gap,<video,audio>subtitles,<has_many_audio>audio,<has_many_video>video,<has_many_edition>editions,<stream>stream-quality,gap,space,play-pause,space,gap,prev,items,next,gap,fullscreen";
          controls_persistency = "paused,idle,audio";
          #   top_bar = "never";
          #   refine = "text_width";
        };
        thumbfast = {
          spawn_first = true;
          network = true;
        };
      };
      bindings = {
        MBTN_LEFT_DBL = "cycle fullscreen";
        MBTN_RIGHT = "cycle pause";
        MBTN_BACK = "playlist-prev";
        MBTN_FORWARD = "playlist-next";

        LEFT = "no-osd seek -5 exact";
        RIGHT = "no-osd seek 5 exact";
        UP = "add volume 2";
        DOWN = "add volume -2";

        "Shift+LEFT" = "no-osd seek -60 exact";
        "Shift+RIGHT" = "no-osd seek 60 exact";

        u = "revert-seek";

        q = "quit-watch-later";
        Q = "quit";

        n = "playlist-next ; script-message playlistmanager show playlist 3";
        p = "playlist-prev ; script-message playlistmanager show playlist 3";

        SPACE = "cycle pause";
        f = "cycle fullscreen";

        "Ctrl+l" = "script-binding uosc/toggle-ui";

        "F" = "script-binding quality_menu/video_formats_toggle";
        "Alt+f" = "script-binding quality_menu/audio_formats_toggle";
      };
    };
  };
}
