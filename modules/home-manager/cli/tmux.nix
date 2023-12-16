{
  pkgs,
  config,
  ...
}: let
  inherit (config.colorScheme) colors;

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
  tmux-nerd-font-window-name = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "tmux-nerd-font-window-name";
    rtpFilePath = "tmux-nerd-font-window-name.tmux";
    version = "unstable-2023-12-13";
    src = pkgs.fetchFromGitHub {
      owner = "joshmedeski";
      repo = "tmux-nerd-font-window-name";
      rev = "410d5becb3a5c118d5fabf89e1633d137906caf1";
      hash = "sha256-HqSaOcnb4oC0AtS0aags2A5slsPiikccUSuZ1sVuago=";
    };
  };
in {
  # xdg.configFile.nvim = {
  #   # TODO: figure out a way to not hard code a path to my nix-dotfiles directory
  #   source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-dotfiles/home-manager/config/nvim";
  #   recursive = true;
  # };
  home.packages = with pkgs; [
    gitmux
    yq-go # required for tmux-nerd-font-window-name
    tmux-sessionizer
  ];
  home.file.".config/tms/default-config.toml".text =
    # toml
    ''
      search_paths = ['${config.home.homeDirectory}/projects']
      default_session = "${config.home.homeDirectory}/nix-dotfiles"
      display_full_path = true
    '';
  home.file.".config/tmux/gitmux.yml".text =
    # yaml
    ''
      tmux:
        symbols:
          branch: "󰘬 "
          hashprefix: ":"
          ahead: ↑·
          behind: ↓·
          staged: "● "
          conflict: " "
          modified: " "
          untracked: "… "
          clean: ✔
          stashed: " "
          insertions: " "
          deletions: " "
        styles:
          state: "#[fg=#${colors.base08},nobold]"
          branch: "#[fg=#${colors.base0E},nobold]"
          staged: "#[fg=#${colors.base0B},nobold]"
          conflict: "#[fg=#${colors.base08},nobold]"
          insertions: "#[fg=#${colors.base0B},nobold]"
          deletions: "#[fg=#${colors.base08},nobold]"
          modified: "#[fg=#${colors.base0A},nobold]"
          untracked: "#[fg=#${colors.base04},nobold]"
          stashed: "#[fg=#${colors.base04},nobold]"
          clean: "#[fg=#${colors.base0B},nobold]"
          divergence: "#[fg=#${colors.base0D},nobold]"
          remote: "#[fg=#${colors.base03},nobold]"
        layout: [flags, " ", "divergence", " ", branch]
        options:
          branch_max_len: 0
          hide_clean: false
          branch_trim: right
    '';
  home.file.".config/tmux/tmux-nerd-font-window-name.yml".text =
    # yaml
    ''
      config:
        fallback-icon: "?"
        multi-pane-icon: ""
        show-name: true
        icon-position: "left"
      icons:
        nvim: ""
        fish: ""
        zsh: ""
        bash: ""
        nh: ""
        python3.10: ""
        python3.11: ""
        python: ""
    '';
  programs.tmux = {
    enable = true;
    shell = "${pkgs.fish}/bin/fish";
    prefix = "M-a";
    extraConfig =
      # bash
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
        set -g status-interval 3         # update the status bar every 3 seconds
        set -g focus-events on           # TODO: learn how this works
        set -g detach-on-destroy off # don't exit from tmux when closing a session
        setw -g automatic-rename on      # automatically rename windows based on the application within
        set -g set-titles on             # use titles in tabs
        set -g set-titles-string "#I:#W" # Set parent terminal title to reflect current window in tmux session
        set -as terminal-overrides ',*:Smulx=\E[4::%p1%dm'  # undercurl support
        set -as terminal-overrides ',*:Setulc=\E[58::2::%p1%{65536}%/%d::%p1%{256}%/%{255}%&%d::%p1%{255}%&%d%;m'  # underscore colours - needs tmux-3.0

        # prefix MAPPINGS
        bind c new-window -c "#{pane_current_path}" # new window and retain cwd
        bind C new-session # new session
        bind d detach-client # detach client
        bind e send-keys "tmux capture-pane -p -S - | nvim -c 'set buftype=nofile' +" Enter
        bind j display-popup -E "tms switch"
        bind o display-popup -E "tms"
        bind r source-file ~/.config/tmux/tmux.conf # Easier reload of config
        bind s split-window -v -c "#{pane_current_path}" # split window horizontally
        bind v split-window -h -c "#{pane_current_path}" # split window vertically
        bind x kill-pane # skip "kill-pane 1? (y/n)" prompt

        # vim MAPPINGS
        not_tmux="ps -o state= -o comm= -t '#{pane_tty}' | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?(g?(view|n?vim?x?)(diff)?|fzf)$'"
        bind-key -n 'C-h' if-shell "$not_tmux" 'send-keys C-h' 'select-pane -R'
        bind-key -n 'C-j' if-shell "$not_tmux" 'send-keys C-j' 'select-pane -D'
        bind-key -n 'C-k' if-shell "$not_tmux" 'send-keys C-k' 'select-pane -U'
        bind-key -n 'C-l' if-shell "$not_tmux" 'send-keys C-l' 'select-pane -L'

        # base MAPPINGS
        bind -n M-j display-popup -E "tms switch"
        bind -n M-o display-popup -E "tms"
        bind -n M-n next-window
        bind -n M-p previous-window

        # copy-mode MAPPINGS
        bind -n M-x copy-mode\; send-keys -X start-of-line\; send-keys -X search-backward '#{prompt}' \; send-keys -X jump-backward '#{prompt}'\; send-keys -X next-space \; send-keys -X cursor-right
        bind-key -T copy-mode-vi 'M-l' select-pane -R
        bind-key -T copy-mode-vi 'M-j' select-pane -D
        bind-key -T copy-mode-vi 'M-k' select-pane -U
        bind-key -T copy-mode-vi 'M-h' select-pane -L

        bind-key -T copy-mode-vi Enter send -X copy-selection-and-cancel
        bind-key -T copy-mode-vi 'v' send -X begin-selection
        bind-key -T copy-mode-vi y send-keys -X copy-selection-no-clear 'reattach-to-user-namespace pbcopy'

        #  ╭──────────────────────────────────────────────────────────╮
        #  │                    joshmedeski theme                     │
        #  ╰──────────────────────────────────────────────────────────╯

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
        # color_statusline="#282828"
        color_statusline="#${colors.base01}"

        set -g status-left "#{tmux_mode_indicator} " # tmux mode
        set -ga status-left "#[fg=$color_purple,bold]#S " # session name
        set -g status-left-length 200
        set -g status-right "#[fg=white,nobold]#(gitmux -cfg $HOME/.config/tmux/gitmux.yml)"
        set -g status-position bottom
        set -g status-style "bg=$color_statusline,fg=$color_fg"

        set -g window-status-format " #[fg=#${colors.base04}]#W "
        set -g window-status-current-format " #[fg=$color_fg,bold]#W*"

        set -g message-command-style bg=default,fg=yellow
        set -g message-style bg=default,fg=yellow
        set -g pane-active-border-style "fg=magenta,bg=default"
        set -g pane-border-style "fg=brightblack,bg=default"

        # set -g popup-border-style "fg=#{color_bg},bg=''${color_bg}"
        # set -g popup-border-lines rounded

        run-shell ${tmux-mode-indicator}/share/tmux-plugins/tmux-mode-indicator/mode_indicator.tmux

        set -g @continuum-save-interval '5'
        set -g @continuum-restore 'on'
        run-shell ${pkgs.tmuxPlugins.continuum}/share/tmux-plugins/continuum/continuum.tmux
      '';
    plugins = with pkgs; [
      tmux-fzf-url
      tmux-nerd-font-window-name
      {
        plugin = tmux-mode-indicator;
        extraConfig =
          # bash
          ''
            # set -g @mode_indicator_prefix_prompt "▍WAIT"
            # set -g @mode_indicator_prefix_mode_style 'fg=green,bold'
            # set -g @mode_indicator_empty_prompt "▍TMUX"
            # set -g @mode_indicator_empty_mode_style 'fg=orange,bold'
            # set -g @mode_indicator_copy_prompt "▍COPY"
            # set -g @mode_indicator_copy_mode_style 'fg=yellow,bold'
            # set -g @mode_indicator_sync_prompt "▍SYNC"
            # set -g @mode_indicator_sync_mode_style 'fg=red,bold'

            set -g @mode_indicator_prefix_prompt "  Wait  "
            set -g @mode_indicator_prefix_mode_style 'bg=orange,fg=#${colors.base00},bold'
            set -g @mode_indicator_empty_prompt "  Tmux  "
            set -g @mode_indicator_empty_mode_style 'bg=green,fg=#${colors.base00},bold'
            set -g @mode_indicator_copy_prompt "  Copy  "
            set -g @mode_indicator_copy_mode_style 'bg=yellow,fg=#${colors.base01},bold'
            set -g @mode_indicator_sync_prompt "  Sync  "
            set -g @mode_indicator_sync_mode_style 'bg=red,fg=#${colors.base00},bold'
          '';
      }
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig =
          # bash
          ''
            set -g @resurrect-save 'S'
            set -g @resurrect-restore 'R'
            set -g @resurrect-capture-pane-contents 'on'

            resurrect_dir="$HOME/.config/tmux/resurrect"
            set -g @resurrect-dir $resurrect_dir
          '';
      }
      # {
      #   plugin = tmuxPlugins.continuum;
      #   extraConfig =
      #     # bash
      #     ''
      #       set -g @continuum-restore 'on'
      #       set -g @continuum-save-interval '5' # minutes
      #       # set -g @continuum-boot 'on'
      #       # set -g @continuum-systemd-start-cmd 'start-server'
      #     '';
      # }
      # ? roosta/tmux-fuzzback
      # ? sainnhe/tmux-fzf
    ];
  };
  programs.fish.interactiveShellInit =
    # fish
    ''
      bind \ej -M insert 'commandline "tms switch" && commandline -f execute && commandline -f repaint'
      bind \eo -M insert 'commandline "tms" && commandline -f execute && commandline -f repaint'
    '';
}
