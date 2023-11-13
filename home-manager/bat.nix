{pkgs, ...}: {
  home.file.".config/bat/themes/gruvbox-dark-hard.tmTheme".source = ./config/bat/themes/gruvbox-dark-hard.tmTheme;
  programs.bat = {
    enable = true;
    # TODO: bat can't find this path for some reason, but yazi does
    # config = {theme = "~/.config/bat/themes/gruvbox-dark-hard.tmTheme";};
  };
  programs.fish.interactiveShellInit = ''
    set -gx BAT_THEME "gruvbox-dark-hard"
  '';
}
