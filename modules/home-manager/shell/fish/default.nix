{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.fish;
in
{
  options.shell.fish = {
    enable = mkEnableOption "Fish";
  };

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      functions = {
        fish_greeting = "";
      };
      plugins = [
        {
          name = "autopair";
          inherit (pkgs.fishPlugins.autopair) src;
        }
        {
          name = "sponge";
          inherit (pkgs.fishPlugins.sponge) src;
        }
      ];
      interactiveShellInit = ''
                        	function vterm_printf;
                            if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
                                # tell tmux to pass the escape sequences through
                                printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
                            else if string match -q -- "screen*" "$TERM"
                                # GNU screen (screen, screen-256color, screen-256color-bce)
                                printf "\eP\e]%s\007\e\\" "$argv"
                            else
                                printf "\e]%s\e\\" "$argv"
                            end
                        end


                function vterm_prompt_end;
                    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
                end
                functions --copy fish_prompt vterm_old_fish_prompt
                function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
                    # Remove the trailing newline from the original prompt. This is done
                    # using the string builtin from fish, but to make sure any escape codes
                    # are correctly interpreted, use %b for printf.
                    printf "%b" (string join "\n" (vterm_old_fish_prompt))
                    vterm_prompt_end
                end


        if [ "$INSIDE_EMACS" = 'vterm' ]
            function clear
                vterm_printf "51;Evterm-clear-scrollback";
                tput clear;
            end
        end
      '';
    };
  };
}
