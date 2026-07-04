{
  description = "Dendritic Nix Configuration";

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    import-tree.url = "github:vic/import-tree";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    kanata-darwin.url = "github:not-in-stock/kanata-darwin";
    kanata-darwin.inputs.nixpkgs.follows = "nixpkgs";

    amzn-community.url = "git+ssh://git.amazon.com:2222/pkg/AmznNix-Community";
    amzn-community.inputs.nixpkgs.follows = "nixpkgs";
    amzn-community.inputs.home-manager.follows = "home-manager";
  };
}
