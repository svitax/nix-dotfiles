# This if your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    inputs.nix-colors.homeManagerModules.default
    inputs.nix-index-database.hmModules.nix-index
    # inputs.neovim-flake.nixosModules."x86_64-linux".hm

    # You can also split up your configuration and import pieces of it here:
    ./features/bat.nix
    ./features/direnv.nix
    ./features/eza.nix
    ./features/firefox.nix
    ./features/fish.nix
    ./features/fonts.nix
    ./features/fzf.nix
    ./features/git.nix
    ./features/helix.nix
    ./features/kitty.nix
    ./features/lf.nix
    ./features/neovim.nix
    ./features/mimeapps.nix
    ./features/papis.nix
    ./features/sioyek.nix
    ./features/starship.nix
    ./features/tmux.nix
    ./features/yazi.nix
    ./features/zellij.nix
    ./features/zoxide.nix
  ];

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-hard;

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.zjstatus

      # You can also add overlays exported from other flakes:
      inputs.neovim-nightly-overlay.overlay
    ];
    # Configure your nixpkgs instance
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "svitax";
    homeDirectory = "/home/svitax";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = with pkgs; [
    zellij-smart-sessionizer
    live-grep # workaround for live-grep in helix until provided by core or plugins
    helix-live-grep # workaround for live-grep in helix until provided by core or plugins
    ripgrep
    fd
    # wl-clipboard # causes weird flickering in neovim + tmux
    xclip
    lazygit
    nurl # prefetch sources for nix build tool
    nix-init # generate nix packages from urls
    ghq
    # TODO: don't use rustup
    rustup
    gcc
  ];

  # Enable home-manager
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  # systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
