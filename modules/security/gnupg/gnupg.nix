{
  ...
}:
{
  flake.modules.nixos.gnupg = {
    programs.gnupg.agent = {
      enable = true;
      settings = {
        default-cache-ttl = 43200;
        max-cache-ttl = 43200;
        allow-emacs-pinentry = "";
        allow-loopback-pinentry = "";
      };
    };
  };
}
