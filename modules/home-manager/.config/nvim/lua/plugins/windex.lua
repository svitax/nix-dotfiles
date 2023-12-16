return {
    {
        "declancm/windex.nvim",
        opts = { default_keymaps = false },
        keys = {
            { "<C-h>", "<cmd>lua require('windex').switch_window('left')<cr>", desc = "Switch window left" },
            { "<C-j>", "<cmd>lua require('windex').switch_window('down')<cr>", desc = "Switch window down" },
            { "<C-k>", "<cmd>lua require('windex').switch_window('up')<cr>", desc = "Switch window up" },
            { "<C-l>", "<cmd>lua require('windex').switch_window('right')<cr>", desc = "Switch window right" },
        },
    },
}
