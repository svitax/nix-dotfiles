return {
    {
        "rest-nvim/rest.nvim",
        ft = { "http" },
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {},
        config = function(_, opts)
            require("rest-nvim").setup(opts)
            vim.keymap.set("n", "<leader>mw", "<Plug>RestNvim", { desc = "Run request" })
        end,
    },
}
