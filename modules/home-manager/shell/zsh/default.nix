{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.zsh;
in
{
  options.shell.zsh = {
    enable = mkEnableOption "Zshell";
  };

  config = lib.mkIf cfg.enable {
    # home.packages = with pkgs; [
    #   hyperfine
    # ];

    programs.zsh = {
      enable = true;
      completionInit = ''
               if [[ -n $(print ~/.zcompdump(Nmh+24)) ]] {
               	# Regenerate completions because the dump file hasn't been modified within the last 24 hours
        	compinit
        } else {
        	# Reuse the existing completions file
        	compinit -C
        }
      '';
      initExtra = ''
               vterm_printf() {
                 if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ]); then
                     # Tell tmux to pass the escape sequences through
                     printf "\ePtmux;\e\e]%s\007\e\\" "$1"
                 elif [ "''${TERM%%-*}" = "screen" ]; then
                     # GNU screen (screen, screen-256color, screen-256color-bce)
                     printf "\eP\e]%s\007\e\\" "$1"
                 else
                     printf "\e]%s\e\\" "$1"
                 fi
        }
      '';
    };
  };
}
