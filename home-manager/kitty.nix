{pkgs, ...}: {
  home.file.".config/kitty/pager.sh".source = ./pager.sh;
  programs.kitty = {
    enable = true;
    shellIntegration.enableFishIntegration = true;
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 16;
    };
    settings = {
      scrollback_pager = "~/.config/kitty/pager.sh";
    };
  };
}
