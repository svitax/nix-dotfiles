{...}: {
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    tmux.enableShellIntegration = true;
    defaultOptions = ["--multi" "--bind 'alt-a:select-all,alt-d:kill-word,alt-t:toggle-all,ctrl-j:accept,ctrl-k:kill-line,ctrl-n:down,ctrl-p:up,up:previous-history,down:next-history'" "--height 90%" "--reverse" "--tiebreak end"];
  };
}
