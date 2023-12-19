return {
    -- "<C-c>" to comment visual regions/lines
    {
        "numToStr/Comment.nvim",
        opts = {
            toggler = { line = "<C-c>", block = "<S-C-c>" },
            opleader = { line = "<C-c>", block = "<S-C-c>" },
        },
    },
}
