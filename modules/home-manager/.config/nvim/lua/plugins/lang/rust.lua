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
    {
        "hrsh7th/nvim-cmp",
        opts = function(_, opts)
            local cmp = require("cmp")
            opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
                { name = "crates" },
            }))
        end,
    },
}
