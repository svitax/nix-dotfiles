{ pkgs, ... }: {
  fonts.fontconfig = { enable = true; };
  home.packages = with pkgs; [
    noto-fonts
    # Nerd Fonts
    (nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" "Monofur" ]; })
  ];
}
