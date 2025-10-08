{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.bash;
in
{
  options.shell.bash = {
    enable = mkEnableOption "Bash";
  };

  config = lib.mkIf cfg.enable {
    home.file.".local/share/my_bash/dircolors".source = ./dircolors;
    programs.bash = {
      enable = true;
      initExtra = ''
        # Shorter version of a common command that it used herein.
        _checkexec ()
        {
            command -v "$1" > /dev/null
        }

        # Default pager.  The check for the terminal is useful for Emacs with
        # M-x shell (which is how I usually interact with bash these days).
        #
        # The COLORTERM is documented in (info "(emacs) General Variables").
        # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
        # Connection Variables").
        if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
        then
            PAGER="cat"
            alias less="cat"
            TERM=dumb-emacs-ansi
            COLORTERM=1
        else
            # Quit once you try to scroll past the end of the file.
            export PAGER="less --quit-at-eof"
        fi

        # Enable automatic color support for common commands that list output
        # and also add handy aliases.  Note the link to the `dircolors`.  This
        # is provided by my dotfiles.
        if _checkexec dircolors
        then
            dircolors_data="$HOME/.local/share/my_bash/dircolors"
            test -r $dircolors_data && eval "$(dircolors -b ''${dircolors_data})" || eval "$(dircolors -b)"
        fi

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
