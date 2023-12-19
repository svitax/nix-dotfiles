return {
    {
        "chrisgrieser/nvim-tinygit",
        event = "VeryLazy",
        ft = { "gitrebase", "gitcommit" },
        dependencies = { { "stevearc/dressing.nvim", opts = { input = { insert_only = false } } } },
        opts = {},
        keys = {
            -- stylua: ignore start
            { "gc", function() require("tinygit").smartCommit() end, desc = "Git commit", },
            { "gC", function() require("tinygit").smartCommit({ pushIfClean = true }) end, desc = "Git commit and push", },
            { "gp", function() require("tinygit").push() end, desc = "Git push", },
            { "gF", function() require("tinygit").fixupCommit({ autoRebase = true }) end, desc = "Git fixup and rebase", },
            { "gm", function() require("tinygit").amendNoEdit({ forcePush = true }) end, desc = "Git ammend-no-edit and f-push", },
            { "gM", function() require("tinygit").amendOnlyMsg({ forcePush = true }) end, desc = "Git ammend only msg and f-push", },
            -- stylua: ignore end
        },
    },
}
