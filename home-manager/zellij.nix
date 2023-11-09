{
  pkgs,
  config,
  ...
}: {
  home.file.".config/zellij/layouts/default.kdl".text = ''
     keybinds {
        shared_except "locked" {
        	bind "Ctrl y" {
     	LaunchOrFocusPlugin "file:~/.config/zellij/plugins/zellij_forgot.wasm" {
     		"lock"                  "ctrl + g"
     		"unlock"                "ctrl + g"
     		"new pane"              "ctrl + p + n"
     		"change focus of pane"  "ctrl + p + arrow key"
     		"close pane"            "ctrl + p + x"
     		"rename pane"           "ctrl + p + c"
     		"toggle fullscreen"     "ctrl + p + f"
     		"toggle floating pane"  "ctrl + p + w"
     		"toggle embed pane"     "ctrl + p + e"
     		"choose right pane"     "ctrl + p + l"
     		"choose left pane"      "ctrl + p + r"
     		"choose upper pane"     "ctrl + p + k"
     		"choose lower pane"     "ctrl + p + j"
     		"new tab"               "ctrl + t + n"
     		"close tab"             "ctrl + t + x"
     		"change focus of tab"   "ctrl + t + arrow key"
     		"rename tab"            "ctrl + t + r"
     		"sync tab"              "ctrl + t + s"
     		"brake pane to new tab" "ctrl + t + b"
     		"brake pane left"       "ctrl + t + ["
     		"brake pane right"      "ctrl + t + ]"
     		"toggle tab"            "ctrl + t + tab"
     		"increase pane size"    "ctrl + n + +"
     		"increase pane size"    "ctrl + n + +"
     		"decrease pane size"    "ctrl + n + -"
     		"increase pane top"     "ctrl + n + k"
     		"increase pane right"   "ctrl + n + l"
     		"increase pane bottom"  "ctrl + n + j"
     		"increase pane left"    "ctrl + n + h"
     		"decrease pane top"     "ctrl + n + K"
     		"decrease pane right"   "ctrl + n + L"
     		"decrease pane bottom"  "ctrl + n + J"
     		"decrease pane left"    "ctrl + n + H"
     		"move pane to top"      "ctrl + h + k"
     		"move pane to right"    "ctrl + h + l"
     		"move pane to bottom"   "ctrl + h + j"
     		"move pane to left"     "ctrl + h + h"
     		"search"                "ctrl + s + s"
     		"go into edit mode"     "ctrl + s + e"
     		"detach session"        "ctrl + o + w"
     		"open session manager"  "ctrl + o + w"
     		"quit zellij"           "ctrl + q"
     		floating true
     	}
     }
        }
        }
        layout {
        	pane
        	pane size=1 borderless=true {
        		plugin location="file:${pkgs.zjstatus}/bin/zjstatus.wasm" {
        		format_left "{mode} #[fg=#89B4FA,bold]{session} {tabs}"
        		format_right "{command_git_branch}"
        		format_space ""

        		border_enabled "false"
        		border_char "-"
        		border_format "#[fg=#6C7086]{char}"
        		border_position "top"

        		hide_frame_for_single_pane "true"

        		mode_normal "#[bg=blue,fg=black,bold] {name} "
        		mode_tmux "#[bg=#ffc387,fg=black,bold] {name} "

        		tab_normal "#[fg=#6C7086] {name} "
        		tab_active "#[fg=#9399B2,bold,italic] {name} "

        		// {command_NAME} needs zellij 0.39.0 or newer
        		command_git_branch_command "git rev-parse --abbrev-ref HEAD"
        		command_git_branch_format "#[fg=blue] {stdout} "
        		command_git_branch_interval "10"
        		}
        	}
        }
  '';
  home.file.".config/zellij/plugins".source = ./zellij-plugins;
  # xdg.configFile."zellij/plugins" = ./zellij-plugins;
  programs.zellij = {
    enable = true;
    settings = {
      default_mode = "normal";
      ui.pane_frames.rounded_corners = true;
      theme = "custom";
      themes.custom = {
        fg = "#${config.colorScheme.colors.base05}"; # 05
        bg = "#${config.colorScheme.colors.base00}"; # 00
        black = "#${config.colorScheme.colors.base03}"; #03
        red = "#${config.colorScheme.colors.base08}"; #08
        green = "#${config.colorScheme.colors.base0B}"; # OB
        yellow = "#${config.colorScheme.colors.base0A}"; #0A
        blue = "#${config.colorScheme.colors.base0D}"; # 0D
        magenta = "#${config.colorScheme.colors.base0E}"; # 0E
        cyan = "#${config.colorScheme.colors.base0C}"; # 0C
        white = "#${config.colorScheme.colors.base07}"; # 07
        orange = "#${config.colorScheme.colors.base09}"; #09
      };
    };
  };
  programs.fish.interactiveShellInit = ''
     	if status is-interactive; and type -q zellij
    	set -gx ZELLIJ_AUTO_ATTACH true
    	set -gx ZELLIJ_AUTO_EXIT true
    	eval (zellij setup --generate-auto-start fish | string collect)
    end
  '';
}
