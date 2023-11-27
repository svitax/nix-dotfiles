{
  pkgs,
  config,
  ...
}: let
  inherit (config.colorScheme) colors;
in {
  programs.zellij = {
    enable = true;
    settings = {
      pane_frames = false;
      # ui.pane_frames.rounded_corners = true;
      # ui.pane_frames.enable = false;
      theme = "custom";
      themes.custom = {
        fg = "#${colors.base05}"; # 05
        bg = "#${colors.base00}"; # 00
        black = "#${colors.base03}"; # 03
        red = "#${colors.base08}"; # 08
        green = "#${colors.base0B}"; # OB
        yellow = "#${colors.base0A}"; #0A
        blue = "#${colors.base0D}"; # 0D
        magenta = "#${colors.base0E}"; # 0E
        cyan = "#${colors.base0C}"; # 0C
        white = "#${colors.base07}"; # 07
        orange = "#${colors.base09}"; # 09
      };
    };
  };
  home.file.".config/zellij/plugins".source = ./config/zellij/plugins;
  home.file.".config/zellij/layouts/default.kdl".text =
    # kdl
    ''
      default_mode "locked"
      keybinds {
        normal {
          bind "Alt a" { SwitchToMode "tmux"; }
        }

        locked {
          bind "Alt a" { SwitchToMode "tmux"; }
        }

        shared_except "normal" "locked" {
          bind "Esc" "Space" "Enter" { SwitchToMode "locked"; }
        }

        tmux {
          bind "Alt a" { SwitchToMode "locked"; }
          bind "s" { NewPane "Right"; SwitchToMode "locked"; }
          bind "v" { NewPane "Down"; SwitchToMode "locked"; }
          bind "r" { SwitchToMode "RenameTab"; }
          bind "h" { MoveFocus "Left"; }
          bind "j" { MoveFocus "Down"; }
          bind "k" { MoveFocus "Up"; }
          bind "l" { MoveFocus "Right"; }
          bind ">" { MovePane; }
          bind "H" { Resize "Left"; }
          bind "J" { Resize "Down"; }
          bind "K" { Resize "Up"; }
          bind "L" { Resize "Right"; }
          bind "n" { GoToNextTab; }
          bind "p" { GoToPreviousTab; }
          bind "c" { NewTab; SwitchToMode "locked"; }
          bind "x" { CloseFocus; SwitchToMode "locked"; }
          bind "z" { ToggleFocusFullscreen; SwitchToMode "locked"; }
          bind "q" { Quit; }
          bind "e" { EditScrollback; SwitchToMode "locked"; }
          bind "S" {
            LaunchOrFocusPlugin "zellij:session-manager" {
              floating true
              move_to_focused_tab true
            }
            SwitchToMode "Normal"
          }
          bind "t" { ToggleFloatingPanes; SwitchToMode "locked"; }
          bind "T" { TogglePaneEmbedOrFloating; SwitchToMode "locked"; }
        }

        RenameTab {
          bind "Alt a" { SwitchToMode "tmux"; }
          bind "Enter" { SwitchToMode "locked"; }
          bind "Esc" { UndoRenameTab; SwitchToMode "locked"; }
        }
      }
      layout {
          pane
          pane size=1 borderless=true {
              plugin location="file:${pkgs.zjstatus}/bin/zjstatus.wasm" {
                format_left "{mode}#[fg=#${colors.base0E},bg=#${colors.base02},bold] {session} {tabs}"
                format_right "#[bg=#${colors.base02}]{command_git_branch}"
                format_space "#[bg=#${colors.base02}]"

                border_enabled "false"
                border_char "-"
                border_format "#[fg=#${colors.base02}]{char}"
                border_position "top"

                // hide_frame_for_single_pane "false"

                mode_normal "#[fg=#${colors.base00},bg=#${colors.base0D},bold] {name} "
                mode_locked "#[fg=#${colors.base00},bg=#${colors.base0B},bold] {name} "
                mode_tmux "#[fg=#${colors.base00},bg=#${colors.base09},bold] {name} "

                tab_normal "#[fg=#${colors.base04},bg=#${colors.base02}] {name} "
                tab_active "#[fg=#${colors.base06},bg=#${colors.base02},bold,italic] {name} "

                // {command_NAME} needs zellij 0.39.0 or newer
                command_git_branch_command "git rev-parse --abbrev-ref HEAD"
                command_git_branch_format "#[fg=#${colors.base04},bg=#${colors.base02},bold]󰘬 {stdout} "
                command_git_branch_interval "10"
              }
          }
      }
    '';
  # programs.fish.interactiveShellInit = ''
  #    	if status is-interactive; and type -q zellij
  #   	set -gx ZELLIJ_AUTO_ATTACH true
  #   	set -gx ZELLIJ_AUTO_EXIT true
  #   	eval (zellij setup --generate-auto-start fish | string collect)
  #   end
  # '';
}
