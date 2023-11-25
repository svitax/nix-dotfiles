return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        nil_ls = {
          settings = {
            ["nil"] = {
              formatting = {
                command = { "alejandra" },
              },
            },
            nix = {
              maxMemoryMB = 8192,
              flake = {
                autoArchive = true,
                autoEvalInputs = true,
              },
            },
          },
        },
      },
    },
  },
}
