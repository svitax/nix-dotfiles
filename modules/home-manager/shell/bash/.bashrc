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
    # I always enable 'server-mode' from inside Emacs, so this
    # connects to an existing frame. It creates a new frame if the
    # current one is too small. It starts a new Emacs instance if
    # there is no server running.
    EDITOR="emacsclient -a emacs -t -r"
    VISUAL="emacsclient -a emacs -t -r"
else
    # Quit once you try to scroll past the end of the file.
    export PAGER="less --quit-at-eof"
fi

# For the Emacs package 'native-complete', if the 'HISTCONTROL' environment
# variable is not set to 'ignorespace' or 'ignoreboth' we get a lot of garbage
# in the shell history. We also need to disable bracketed-paste.
export HISTCONTROL=ignoreboth
bind 'set enable-bracketed-past off'
