return {
    {
        -- Incremental LSP renaming based on Neovim's command-preview feature
        "smjonas/inc-rename.nvim",
        cmd = "IncRename",
        opts = {},
        keys = {
            {
                "<leader>r",
                function()
                    return ":IncRename " .. vim.fn.expand("<cword>")
                end,
                expr = true,
                desc = "Rename symbol",
            },
        },
    },
}
