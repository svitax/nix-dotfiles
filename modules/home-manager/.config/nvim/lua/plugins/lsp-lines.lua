return {
    {
        -- BUG: virtual_lines = { highlight_whole_line = false } gets reset whenever you do require("lsp_lines").toggle()
        -- so we can't have lsp_lines disabled by default and have highlight_whole_line = false at the same time
        -- This bug was introduced here https://git.sr.ht/~whynothugo/lsp_lines.nvim/commit/512d8c79637e7aeb889240c2e0ca8ae327940737
        "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        commit = "84578d4a6c91d7db25e1a47727f5b04177a77011",
        event = "LspAttach",
        config = function(_, opts)
            vim.diagnostic.config({ virtual_lines = false })
            require("lsp_lines").setup()
        end,
        keys = {
            {
                "<leader>l",
                function()
                    require("lsp_lines").toggle()
                end,
                desc = "Toggle virtual diagnostic lines",
            },
        },
    },
}
