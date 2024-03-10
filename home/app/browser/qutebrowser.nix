{ pkgs, lib, config, ... }:
let inherit (config.colorScheme) colors;
in {
  programs.qutebrowser = {
    enable = true;
    keyBindings = {
      normal = {
        "gt" = "tab-next";
        "gT" = "tab-prev";
        "gn" = "tab-clone";
        "gN" = "tab-close";
        "<Ctrl-l>" = "edit-url";
      };
      command = {
        "<Ctrl-n>" = "completion-item-focus next";
        "<Ctrl-p>" = "completion-item-focus prev";
        "<Up>" = "completion-item-focus --history prev";
        "<Down>" = "completion-item-focus --history next";
      };
    };
    quickmarks = {
      "nixos/options" = "https://search.nixos.org/options";
      "nixos/packages" = "https://search.nixos.org/packages";
      "nixos/home-manager" = "https://github.com/nix-community/home-manager";
    };
    searchEngines = {
      DEFAULT = "https://google.com/search?hl=en&q={}";
      ddg = "https://duckduckgo.com/?q={}";
      g = "https://www.google.com/search?hl=en&q={}";
      yt = "https://youtube.com/results?search_query={}";
      no =
        "https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&type=packages&query={}";
      np =
        "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query={}";
      ho = "https://mipmip.github.io/home-manager-option-search/?query={}";
      gh = "https://github.com/search?q={}";
    };
    settings = {
      fonts = {
        default_family = "JetBrains Mono Nerd Font";
        default_size = "16pt";
        web = { size.default = 16; };
      };
      zoom.default = "125%";
      content = {
        autoplay = false;
        pdfjs = false;
        cookies = {
          accept = "no-3rdparty";
          store = true;
        };
        geolocation = false;
        hyperlink_auditing = true;
        images = true;
        media = {
          audio_capture = false;
          audio_video_capture = false;
          video_capture = false;
        };
        notifications = { enabled = false; };
        prefers_reduced_motion = true;
      };
      completion.height = "33%";
      editor.command = [ "emacsclient" "-c" "{file}" ];
      downloads = { remove_finished = 10 * 1000; };
      tabs = {
        favicons.show = "never";
        last_close = "close";
        show = "multiple";
      };
      colors = {
        completion = {
          fg = "#${colors.base05}";
          odd.bg = "#${colors.base00}";
          even.bg = "#${colors.base00}";
          category = {
            fg = "#${colors.base0A}";
            bg = "#${colors.base00}";
            border = {
              top = "#${colors.base00}";
              bottom = "#${colors.base00}";
            };
          };
          item = {
            selected = {
              fg = "#${colors.base05}";
              bg = "#${colors.base02}";
              border = {
                top = "#${colors.base02}";
                bottom = "#${colors.base02}";
              };
              match = { fg = "#${colors.base0C}"; };
            };
          };
          match = { fg = "#${colors.base0C}"; };
          scrollbar = {
            fg = "#${colors.base05}";
            bg = "#${colors.base00}";
          };
        };
        contextmenu = {
          disabled = {
            bg = "#${colors.base01}";
            fg = "#${colors.base04}";
          };
          menu = {
            bg = "#${colors.base00}";
            fg = "#${colors.base05}";
          };
          selected = {
            bg = "#${colors.base02}";
            fg = "#${colors.base05}";
          };
        };
        prompts = {
          bg = "#${colors.base00}";
          fg = "#${colors.base05}";
          border = "1px solid #${colors.base00}";
        };
        messages = {
          error = {
            fg = "#${colors.base00}";
            bg = "#${colors.base08}";
            border = "#${colors.base08}";
          };
          info = {
            fg = "#${colors.base05}";
            bg = "#${colors.base00}";
            border = "#${colors.base00}";
          };
          warning = {
            fg = "#${colors.base00}";
            bg = "#${colors.base0E}";
            border = "#${colors.base0E}";
          };
        };
        downloads = {
          start = {
            fg = "#${colors.base01}";
            bg = "#${colors.base09}";
          };
          stop = {
            fg = "#${colors.base01}";
            bg = "#${colors.base0B}";
          };
          error = {
            fg = "#${colors.base06}";
            bg = "#${colors.base08}";
          };
          system = {
            bg = "rgb";
            fg = "rgb";
          };
        };
        hints = {
          # bg = "#${colors.base0A}BF";
          bg = "rgba(235,203,139,0.8)";
          fg = "#${colors.base01}";
          match.fg = "#${colors.base0B}";
        };
        keyhint = {
          fg = "#${colors.base04}";
          bg = "#${colors.base01}";
          suffix.fg = "#${colors.base0A}";
        };
        statusbar = {
          normal = {
            bg = "#${colors.base00}";
            fg = "#${colors.base0B}";
          };
          insert = {
            bg = "#${colors.base0D}";
            fg = "#${colors.base00}";
          };
          passthrough = {
            bg = "#${colors.base0C}";
            fg = "#${colors.base00}";
          };
          caret = {
            bg = "#${colors.base0E}";
            fg = "#${colors.base00}";
          };
          command = {
            bg = "#${colors.base00}";
            fg = "#${colors.base05}";
          };
          private = {
            bg = "#${colors.base01}";
            fg = "#${colors.base00}";
          };
          progress.bg = "#${colors.base0D}";
          url = {
            fg = "#${colors.base05}";
            success.http.fg = "#${colors.base0C}";
            success.https.fg = "#${colors.base0B}";
            hover.fg = "#${colors.base05}";
            error.fg = "#${colors.base08}";
            warn.fg = "#${colors.base0E}";
          };
        };
        tabs = {
          bar.bg = "#${colors.base00}";
          even.bg = "#${colors.base01}";
          even.fg = "#${colors.base05}";
          odd.bg = "#${colors.base01}";
          odd.fg = "#${colors.base05}";
          indicator = {
            error = "#${colors.base08}";
            stop = "#${colors.base0C}";
            start = "#${colors.base0D}";
          };
          pinned = {
            even.bg = "#${colors.base0C}";
            even.fg = "#${colors.base07}";
            odd.bg = "#${colors.base0B}";
            odd.fg = "#${colors.base07}";
            selected = {
              even.bg = "#${colors.base02}";
              even.fg = "#${colors.base05}";
              odd.bg = "#${colors.base02}";
              odd.fg = "#${colors.base05}";
            };
          };
          selected = {
            even.bg = "#${colors.base02}";
            even.fg = "#${colors.base05}";
            odd.bg = "#${colors.base02}";
            odd.fg = "#${colors.base05}";
          };
        };
        webpage = {
          darkmode.enabled = false;
          preferred_color_scheme = "dark";
          bg = "#${colors.base00}";
        };
      };
    };
  };
}
