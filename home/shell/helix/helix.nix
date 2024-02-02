{ ... }: {
  home.file.".config/helix".source = ./.;
  programs.helix = {
    enable = true;
    # extraPackages = [];
  };
}
