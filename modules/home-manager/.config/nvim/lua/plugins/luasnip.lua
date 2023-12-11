return {
    -- Snippet engine
    {
        "L3MON4D3/LuaSnip",
        event = "InsertEnter",
        opts = {
            -- Don't store snippet history for less overhead
            history = false,
            -- Event on which to check for exiting a snippet's region
            region_check_events = "InsertEnter",
            delete_check_events = "InsertLeave",
            ft_func = function()
                return vim.split(vim.bo.filetype, ".", { plain = true })
            end,
            config = function(_, opts)
                require("luasnip").setup(opts)
                -- require("luasnip.loaders.from_vscode").lazy_load({ paths = "./snippets" })
                -- require("luasnip.loaders.from_lua").lazy_load({ paths = "./snippets/luasnippets" })
            end,
        },
    },
}
