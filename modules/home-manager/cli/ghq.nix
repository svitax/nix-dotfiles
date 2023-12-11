{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    ghq
  ];
  programs.git.extraConfig = {
    ghq.root = "${config.home.homeDirectory}/projects";
  };
  programs.fish.interactiveShellInit = ''
    set -gx GHQ_ROOT "${config.home.homeDirectory}/projects"
  '';
  programs.fish.shellAbbrs = {
    gget = "ghq get";
    gup = "ghq get --update";
    glist = "ghq list";
    gcr = "ghq create";
  };
  programs.fish.functions = {
    gcd = {body = ''cd $GHQ_ROOT/$(ghq list | fzf)'';};
  };
}
