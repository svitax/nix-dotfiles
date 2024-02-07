{ pkgs, ... }: {
  # console = {
  #   font = "ter-v18b";
  #   packages = [ pkgs.terminus_font ];
  # };
  services.greetd = {
    enable = true;
    settings = {
      default_session.command = ''
        ${pkgs.greetd.tuigreet}/bin/tuigreet \
        --time \
        --asterisks \
        --user-menu \
        --remember \
        --cmd sway
      '';
    };
  };
  environment.etc."greetd/environments".text = ''
    sway
  '';
}
