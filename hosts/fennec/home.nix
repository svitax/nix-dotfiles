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
    ../../modules/home-manager/colors.nix
    ../../modules/home-manager/fonts.nix

    ../../modules/home-manager/desktop/firefox.nix
    ../../modules/home-manager/desktop/kitty.nix
    ../../modules/home-manager/desktop/sioyek.nix

    ../../modules/home-manager/cli/bat.nix
    ../../modules/home-manager/cli/direnv.nix
    ../../modules/home-manager/cli/eza.nix
    ../../modules/home-manager/cli/fish.nix
    ../../modules/home-manager/cli/fzf.nix
    ../../modules/home-manager/cli/ghq.nix
    ../../modules/home-manager/cli/git.nix
    ../../modules/home-manager/cli/helix.nix
    ../../modules/home-manager/cli/lf.nix
    ../../modules/home-manager/cli/neovim.nix
    ../../modules/home-manager/cli/mimeapps.nix
    ../../modules/home-manager/cli/papis.nix
    ../../modules/home-manager/cli/starship.nix
    ../../modules/home-manager/cli/tmux.nix
    # ../../modules/home-manager/cli/zellij.nix
    ../../modules/home-manager/cli/zoxide.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      # outputs.overlays.zjstatus

      # You can also add overlays exported from other flakes:
      inputs.neovim-nightly-overlay.overlay
      # inputs.nvim-fennec.overlays.default
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
    # nvim-fennec # nvim-flake output
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
    # TODO: don't use rustup
    rustup
    gcc
  ];

  # Enable home-manager
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
