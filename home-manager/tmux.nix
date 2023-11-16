{
  pkgs,
  lib,
  config,
  ...
}: let
  colors = config.colorScheme.colors;

  t-smart-tmux-session-manager = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "t-smart-tmux-session-manager";
    rtpFilePath = "t-smart-tmux-session-manager.tmux";
    version = "unstable-2023-11-15";
    src = pkgs.fetchFromGitHub {
      owner = "joshmedeski";
      repo = "t-smart-tmux-session-manager";
      rev = "0fa0d6be8996c0bfd030fc67e49e782727375671";
      sha256 = "sha256-B+NPeR0BZMX4wFtNK3M7shF2T5arXdIrFcVDRvplUT8=";
    };
  };
  tmux-mode-indicator = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-mode-indicator";
    rtpFilePath = "mode_indicator.tmux";
    version = "unstable-2023-11-15";
    src = pkgs.fetchFromGitHub {
      owner = "MunifTanjim";
      repo = "tmux-mode-indicator";
      rev = "7027903adca37c54cb8f5fa99fc113b11c23c2c4";
      sha256 = "sha256-SAzsn4LoG8Ju5t13/U3/ctlJQPyPgv2FjpPkWSeKbP0=";
    };
  };
  tmux-fzf-url = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-fzf-url";
    rtpFilePath = "fzf-url.tmux";
    version = "unstable-2023-11-15";
    src = pkgs.fetchFromGitHub {
      owner = "joshmedeski";
      repo = "tmux-fzf-url";
      rev = "f67ed87a25e26ed397559542b574f83a9ddf6aed";
      sha256 = "sha256-xMpasqXlLeTYL/TVnEfRHsawZDLONVyp5pn3Q8HFzLA=";
    };
  };
in {
  # xdg.configFile.nvim = {
  #   # TODO: figure out a way to not hard code a path to my nix-dotfiles directory
  #   source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-dotfiles/home-manager/config/nvim";
  #   recursive = true;
  # };
  programs.tmux = {
    enable = true;
    shell = "${pkgs.fish}/bin/fish";
    prefix = "C-a";
    extraConfig =
      /*
      sh
      */
      ''
        set -g default-terminal "xterm-kitty"
        set -ga terminal-overrides ",*256col*:Tc"
        setenv -g COLORTERM truecolor

        set-option -g allow-passthrough
        set-option -ga update-environment TERM
        set-option -ga update-environment TERM_PROGRAM

        set -g base-index 1              # start indexing windows at 1 instead of 0
        setw -g pane-base-index 1        # start indexing panes at 1 instead of 0
        set -g detach-on-destroy off     # don't exit from tmux when closing a session
        set -g escape-time 0             # zero-out escape time delay (http://superuser.com/a/252717/65504) a larger value may be required in remote connections
        set -g history-limit 1000000     # increase history size (from 2,000)
        set -g mouse on                  # enable mouse support
        set -g renumber-windows on       # renumber all windows when any window is closed
        set -g set-clipboard on          # use system clipboard
        set -g status-interval 1         # update the status bar every 3 seconds
        set -g focus-events on           # TODO: learn how this works
        setw -g automatic-rename on      # automatically rename windows based on the application within
        set -g set-titles on             # use titles in tabs
        set -g set-titles-string "#I:#W" # Set parent terminal title to reflect current window in tmux session
        set -as terminal-overrides ',*:Smulx=\E[4::%p1%dm'  # undercurl support
        set -as terminal-overrides ',*:Setulc=\E[58::2::%p1%{65536}%/%d::%p1%{256}%/%{255}%&%d::%p1%{255}%&%d%;m'  # underscore colours - needs tmux-3.0

        # Easier reload of config
        bind o source-file ~/.config/tmux/tmux.conf

        #  ╭──────────────────────────────────────────────────────────╮
        #  │                    joshmedeski theme                     │
        #  ╰──────────────────────────────────────────────────────────╯

        # TODO: use nix-colors for this
        color_bg="#${colors.base00}"
        color_fg="#${colors.base05}"
        color_green="#${colors.base0B}"
        color_yellow="#${colors.base0A}"
        color_red="#${colors.base08}"
        color_blue="#${colors.base0D}"
        color_cyan="#${colors.base0C}"
        color_purple="#${colors.base0E}"
        color_gray="#${colors.base02}"
        color_light_gray="#${colors.base03}"
        # color_statusline="#171a1a"
        color_statusline="#${colors.base01}"

        set -g status-style "bg=$color_statusline,fg=$color_fg" # transparent status bar
        set -g status-position bottom
        set -g status-justify centre

        set -g status-left-length 90
        set -g status-left "#{tmux_mode_indicator} " # tmux mode
        set -ga status-left "#[fg=$color_light_gray,bold]#S " # session name
        # set -ga status-left "#{pomodoro_status}" # pomodoro

        set -g status-right-length 90
        # set -g status-right "#(gitmux -cfg $HOME/.config/tmux/gitmux.conf '#{pane_current_path}')"
        set -g status-right ""

        setw -g window-status-format "#[fg=$color_light_gray,bg=default]#W"
        setw -g window-status-current-format "#[fg=$color_fg,bold]#W"

        setw -g window-status-separator "  "
        set-window-option -g mode-style bg=$color_purple,fg=$color_bg
        set -g message-style bg=$color_blue,fg=$color_bg

        set -g pane-active-border-style "fg=$color_fg,bg=default"
        set -g pane-border-style "fg=brightblack,bg=default"

        # set -g popup-border-style "fg=#{color_bg},bg=''${color_bg}"
        # set -g popup-border-lines rounded

        run-shell ${tmux-mode-indicator}/share/tmux-plugins/tmux-mode-indicator/mode_indicator.tmux
        # bind -n C-n next-window
        # bind -n C-p previous-window
      '';
    plugins = with pkgs; [
      tmux-fzf-url
      {
        plugin = tmux-mode-indicator;
        extraConfig = ''
          # TODO: use nix-colors for this?
          set -g @mode_indicator_prefix_prompt "▍WAIT"
          set -g @mode_indicator_prefix_mode_style 'fg=green,bold'
          set -g @mode_indicator_empty_prompt "▍TMUX"
          set -g @mode_indicator_empty_mode_style 'fg=orange,bold'
          set -g @mode_indicator_copy_prompt "▍COPY"
          set -g @mode_indicator_copy_mode_style 'fg=yellow,bold'
          set -g @mode_indicator_sync_prompt "▍SYNC"
          set -g @mode_indicator_sync_mode_style 'fg=red,bold'
        '';
      }
      {
        plugin = t-smart-tmux-session-manager;
        extraConfig = ''
          set -g @t-bind 'f'
          set -g @t-fzf-find-binding 'ctrl-b:change-prompt( )+reload(fd -H -d 2 -t d -E .Trash . ~)'
          set -g @t-fzf-prompt '  '

          # change default fzf results
          # set -g @t-fzf-default-results 'sessions' # show tmux sessions by default
          # set -g @t-fzf-default-results 'zoxide' # show zoxide results by default

          bind -n M-f run-shell "t" # session switcher
        '';
      }
      # {
      #   plugin = tmuxPlugins.resurrect;
      #   extraConfig = ''
      #     resurrect_dir="$HOME/.tmux/resurrect"
      #     set -g @resurrect-dir $resurrect_dir
      #   '';
      # }
      # {
      #   plugin = tmuxPlugins.continuum;
      #   extraConfig = ''
      #     set -g @continuum-restore 'on'
      #     # set -g @continuum-boot 'on'
      #     # set -g @continuum-save-interval '10'
      #     # set -g @continuum-systemd-start-cmd 'start-server'
      #   '';
      # }
      # ? joshmedeski/tmux-nerd-font-window-name
      # ? roosta/tmux-fuzzback
      # ? sainnhe/tmux-fzf
    ];
  };
  programs.fish.interactiveShellInit =
    /*
    fish
    */
    ''
      fish_add_path ${t-smart-tmux-session-manager}/share/tmux-plugins/t-smart-tmux-session-manager/bin/

      set -Ux T_SESSION_USE_GIT_ROOT true # use git root for session name
      set -Ux T_SESSION_NAME_INCLUDE_PARENT true # include parent dir in session name
    '';
}
