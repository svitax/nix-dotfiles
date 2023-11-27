return {
  { "mfussenegger/nvim-lint", opts = { linters_by_ft = { nix = { "statix" } } } },
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        nil_ls = {
          settings = {
            ["nil"] = {
              nix = {
                maxMemoryMB = 4096,
                flake = {
                  autoArchive = true,
                  autoEvalInputs = true,
                  nixpkgsInputName = "nixpkgs",
                },
              },
              formatting = {
                command = { "alejandra" },
              },
            },
          },
        },
      },
    },
  },
}
