return {
    {
        "shellRaining/hlchunk.nvim",
        event = { "UIEnter" },
        opts = function()
            local palette = require("nightfox.palette").load("gruvfox")
            return {
                exclude_filetypes = {
                    NeogitStatus = true,
                    NeogitCommitMessage = true,
                    NeogitConsole = true,
                },
                chunk = { style = palette.magenta.base, notify = false },
                indent = { enable = false },
                line_num = { enable = false },
                blank = { enable = false },
            }
        end,
    },
}
