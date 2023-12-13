return {
    {
        "kylechui/nvim-surround",
        opts = {
            aliases = {
                ["b"] = { ")", "]", "}" },
                ["c"] = "}", -- [c]urly brace
                ["r"] = "]", -- [r]ectangular bracket
                ["e"] = "`", -- t[e]mplate string
                ["q"] = { '"', "'", "`" }, -- [q]uotes
            },
            highlight = { duration = 1000 },
            keymaps = {
                insert = false,
                insert_line = false,
                normal = false,
                normal_cur = false,
                normal_line = false,
                normal_cur_line = false,
                visual = false,
                visual_line = false,
                delete = false,
                change = false,
                change_line = false,
            },
        },
        config = function(_, opts)
            require("nvim-surround").setup(opts)

            vim.keymap.set({ "o", "x" }, "ms", "<Plug>(nvim-surround-visual)", { desc = "Surround add" })

            vim.keymap.set("n", "ms", "<Plug>(nvim-surround-normal)", { desc = "Surround add" })
            vim.keymap.set("n", "ml", "<Plug>(nvim-surround-normal-cur)", { desc = "Surround add (current line)" })
            vim.keymap.set("n", "md", "<Plug>(nvim-surround-delete)", { desc = "Surround delete" })
            vim.keymap.set("n", "mr", "<Plug>(nvim-surround-change)", { desc = "Surround replace" })
        end,
    },
}
