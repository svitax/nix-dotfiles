return {
    {
        "epwalsh/obsidian.nvim",
        version = "*",
        lazy = true,
        event = {
            "BufReadPre " .. vim.fn.expand("~") .. "/OneDrive/Apps/remotely-save/obsidian-vault/**.md",
            "BufNewFile " .. vim.fn.expand("~") .. "/OneDrive/Apps/remotely-save/obsidian-vault/**.md",
        },
        opts = {
            -- dir = "~/OneDrive/Apps/remotely-save/obsidian-vault/",
            workspaces = {
                { name = "personal", path = "~/OneDrive/Apps/remotely-save/obsidian-vault/" },
            },
            mappings = {},
        },
        keys = {
            { "<leader>n", "<cmd>ObsidianQuickSwitch<cr>", desc = "Open notes picker" },
            { "gd", "<cmd>ObsidianFollowLink<cr>", desc = "Follow obsidian link", buffer = true, silent = true },
        },
    },
}
