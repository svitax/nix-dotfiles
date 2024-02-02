return {
    {
        "mrcjkb/rustaceanvim",
        version = "^3",
        ft = { "rust" },
        dependencies = { "lvimuser/lsp-inlayhints.nvim" },
        init = function()
            vim.g.rustaceanvim = {
                server = {
                    settings = {
                        ["rust-analyzer"] = {
                            assist = {
                                importEnforceGranularity = true,
                                importPrefix = "create",
                            },
                            cargo = {
                                allFeatures = true,
                            },
                            checkOnSave = {
                                command = "clippy",
                            },
                        },
                    },
                },
                -- TODO: nvim-dap doesn't pick up the adapter from this plugin
                dap = {},
            }
        end,
    },
    {
        "saecki/crates.nvim",
        event = { "BufRead Cargo.toml" },
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = { src = { cmp = { enabled = true } } },
    },
    {
        "hrsh7th/nvim-cmp",
        dependencies = { "ryo33/nvim-cmp-rust" },
        opts = function(_, opts)
            local cmp = require("cmp")
            opts.sorting.comparators = vim.list_extend({
                -- deprioritize `.box`, `.mut`, etc.
                require("cmp-rust").deprioritize_postfix,
                -- deprioritize `Borrow::borrow` and `BorrowMut::borrow_mut`
                require("cmp-rust").deprioritize_borrow,
                -- deprioritize `Deref::deref` and `DerefMut::deref_mut`
                require("cmp-rust").deprioritize_deref,
                -- deprioritize `Into::into`, `Clone::clone`, etc.
                require("cmp-rust").deprioritize_common_traits,
            }, opts.sorting.comparators)
            opts.sources = cmp.config.sources(vim.list_extend(opts.sources, {
                { name = "crates" },
            }))
        end,
    },
}
