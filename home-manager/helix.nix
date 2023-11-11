{
  pkgs,
  config,
  ...
}: {
  home.file.".config/helix".source = ./config/helix;
  programs.helix = {
    enable = true;
    extraPackages = [pkgs.nodePackages.pyright pkgs.nil];
  };
}
