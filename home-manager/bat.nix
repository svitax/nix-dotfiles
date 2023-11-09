{pkgs, ...}: {
  home.file.".config/bat/themes/gruvbox-dark-hard.tmTheme".source = ./gruvbox-dark-hard.tmTheme;
  programs.bat = {
    enable = true;
    config = {theme = "~/.config/bat/themes/gruvbox-dark-hard.tmTheme";};
  };
}
