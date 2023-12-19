return {
    -- "<C-c>" to comment visual regions/lines
    {
        "numToStr/Comment.nvim",
        opts = {
            toggler = { line = "<C-c>", block = "<S-C-c>" },
            opleader = { line = "<C-c>", block = "<S-C-c>" },
            extra = { above = "<S-C-c>cO", below = "<S-C-c>co", eol = "<S-C-c>cA" },
        },
    },
}
