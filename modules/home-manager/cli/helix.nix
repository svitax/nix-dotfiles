{ ... }: {
  home.file.".config/helix".source = ../.config/helix;
  programs.helix = {
    enable = true;
    # extraPackages = [];
  };
}
