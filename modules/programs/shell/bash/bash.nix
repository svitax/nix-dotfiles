{
  ...
}:
{
  flake.modules.homeManager.bash =
    { pkgs, ... }:
    {
      home.file.".local/share/my_bash/dircolors".source = ./dircolors;

      home.packages = with pkgs; [
        fd
        libqalculate
        ripgrep
      ];

      programs.bash = {
        enable = true;
        initExtra = ''
          _checkexec ()
          {
              command -v "$1" > /dev/null
          }

          if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
          then
              PAGER="cat"
              alias less="cat"
              TERM=dumb-emacs-ansi
              COLORTERM=1
          else
              export PAGER="less --quit-at-eof"
          fi

          if _checkexec dircolors
          then
              dircolors_data="$HOME/.local/share/my_bash/dircolors"
              test -r $dircolors_data && eval "$(dircolors -b ''${dircolors_data})" || eval "$(dircolors -b)"
          fi

          vterm_printf() {
            if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ]); then
                printf "\ePtmux;\e\e]%s\007\e\\" "$1"
            elif [ "''${TERM%%-*}" = "screen" ]; then
                printf "\eP\e]%s\007\e\\" "$1"
            else
                printf "\e]%s\e\\" "$1"
            fi
          }
        '';
      };
    };
}
