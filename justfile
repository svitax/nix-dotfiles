fennec: (_nixos-switch "nixos")

update-neovim:
  echo 'Updating'
  nix flake update ./modules/neovim

update: update-neovim
  echo 'Updating Neovim module'
  nix flake update

_nixos-switch HOST: update
  echo 'Switching'
  nixos-rebuild --use-remote-sudo switch --flake ".#{{HOST}}"
