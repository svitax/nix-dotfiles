return {
    {
        "mrcjkb/rustaceanvim",
        version = "^3",
        ft = { "rust" },
        dependencies = { "lvimuser/lsp-inlayhints.nvim" },
    },
    {
        "saecki/crates.nvim",
        event = { "BufRead Cargo.toml" },
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = { src = { cmp = { enabled = true } } },
    },
}
