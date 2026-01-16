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
          mpris
          uosc
          sponsorblock-minimal
          thumbfast
          quality-menu
          mpv-playlistmanager
          reload
          autoload
          mpv-image-viewer.freeze-window
        ];
      };
      # TODO extract out the .conf files for my mpv config so it is easier to
      # migrate later
      config = {
        ### Seeking ###
        save-position-on-quit = true; # saves the seekbar position on exit
        force-seekable = true; # force videos to be seekable

        ### RAM ###
        cache = true; # uses a large seekable RAM cache even for local input
        cache-secs = 300; # uses extra large RAM cache (needs cache=yes)
        demuxer-max-back-bytes = "20M"; # sets fast seeking
        demuxer-max-bytes = "20M"; # sets fast seeking
        # demuxer-max-bytes = "1800M";
        # demuxer-max-back-bytes = "1200M";
        # stream-buffer-size = "3MiB";

        ### Video ###
        vlang = "en,eng"; # sets the video language
        vo = "gpu-next"; # sets video out to experimental libplacebo renderer
        video-sync = "display-resample";
        interpolation = true;
        scale-antiring = 0.6;
        profile = "high-quality";

        ### Audio ###
        volume-max = 100; # everything above 100 results in amplification
        volume = 70; # default volume
        alang = "en,eng,es,ja,jp,jpn"; # sets the audio language

        ### General ###
        keep-open = true; # keep player open after a video/playlist ends
        pause = true; # disables autoplay
        prefetch-playlist = true; # prefetch/buffer files later in the playlist
        save-watch-history = true;
        write-filename-in-watch-later-config = true;
        watch-later-directory = "${config.xdg.stateHome}/mpv/watch_later";

        ### Hardware Decoder ###
        hwdec = "auto-safe"; # safe list of auto hardware decoder profiles

        ### OSD ###

        ### Subtitles ###
        # sub-auto = "fuzzy";
        # slang = "en,eng,enUS,en-US,es"; # sets the subtitle language
        sub-use-margins = true;
        sub-ass-force-margins = true;
        sub-font-size = 34;
        sub-color = "#FFFFFF";
        sub-border-size = 2.4;
        sub-border-color = "#FF000000";
        sub-shadow-color = "#A0000000";
        sub-shadow-offset = 0.75;
        sub-bold = true;
        sub-ass-style-overrides = "Kerning=yes";
        embedded-fonts = true; # use embedded fonts for SSA/ASS subs

        ### Shaders ###
        # For high-end hardware, we recommend using the following shaders:
        # - ArtCNN_C4F32.glsl <https://github.com/Artoriuz/ArtCNN/blob/main/GLSL/ArtCNN_C4F32.glsl>
        #   for higher quality sources
        # - nnedi3-nns128-win8x4.hook <https://github.com/bjin/mpv-prescalers/blob/master/compute/nnedi3-nns128-win8x4.hook>
        #   for lower quality sources
        # g cycle-values glsl-shaders "~~/shaders/nnedi3-nns128-win8x4.hook" "~~/shaders/ArtCNN_C4F32.glsl" ""
        #
        # or https://github.com/hl2guide/better-mpv-config/blob/c9ff805c47561f9bc2726cee3a325f11decf12df/mpv_v3/configs/mpv_shaders.conf

        ytdl-raw-options = ''sub-lang="en.*,es",sub-format=best'';
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
          hwdec = true;
        };
        ytdl_hook = {
          ytdl_path = "${lib.getExe pkgs.yt-dlp}";
          try_ytdl_first = true;
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

        "Ctrl+h" = "script-binding uosc/keybinds";

        SPACE = "cycle pause";
        f = "cycle fullscreen";

        "Ctrl+l" = "script-binding uosc/toggle-ui";

        "F" = "script-binding quality_menu/video_formats_toggle";
        "Alt+f" = "script-binding quality_menu/audio_formats_toggle";
      };
    };
  };
}
