return {
    {
        "declancm/windex.nvim",
        opts = { default_keymaps = false },
        keys = {
            { "<A-h>", "<cmd>lua require('windex').switch_window('left')<cr>", desc = "Switch window left" },
            { "<A-j>", "<cmd>lua require('windex').switch_window('down')<cr>", desc = "Switch window down" },
            { "<A-k>", "<cmd>lua require('windex').switch_window('up')<cr>", desc = "Switch window up" },
            { "<A-l>", "<cmd>lua require('windex').switch_window('right')<cr>", desc = "Switch window right" },
        },
    },
}
