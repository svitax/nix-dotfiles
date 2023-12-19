return {
    { "LnL7/vim-nix" },
    { "mfussenegger/nvim-lint", opts = { linters_by_ft = { nix = { "statix" } } } },
    { "stevearc/conform.nvim", opts = { formatters_by_ft = { nix = { "nixfmt" } } } },
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
                        },
                    },
                },
            },
        },
    },
}
