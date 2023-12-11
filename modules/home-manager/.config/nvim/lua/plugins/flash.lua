return {
    -- Navigate code with search labels and enchanced character motions
    {
        -- TODO: Telescope integration
        -- TODO: flash hg don't go italic
        "folke/flash.nvim",
        opts = {
            modes = {
                search = { enabled = false },
                jump = { autojump = true },
                treesitter = { label = { after = false } },
                treesitter_search = { label = { after = false } },
            },
        },
        keys = {
            { "f" },
            { "F" },
            { "t" },
            { "t" },
            {
                "s",
                mode = { "n", "x", "o" },
                function()
                    require("flash").jump()
                end,
                desc = "flash",
            },
            {
                "<S-CR>",
                mode = { "n", "x", "o" },
                function()
                    require("flash").treesitter_search()
                end,
                desc = "flash treesitter search",
            },
        },
    },
}
