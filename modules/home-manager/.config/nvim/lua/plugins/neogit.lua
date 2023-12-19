return {
    {
        "NeogitOrg/neogit",
        cmd = "Neogit",
        opts = function()
            local icons = require("utils.icons")

            return {
                console_timeout = 10000,
                auto_show_console = true,
                disable_hint = true,
                disable_commit_confirmation = true,
                disable_insert_on_commit = false,
                kind = "replace",
                use_per_project_settings = true,
                remember_settings = true,
                graph_style = "unicode",
                ignored_settings = {
                    "NeogitPushPopup--force-with-lease",
                    "NeogitPushPopup--force",
                    "NeogitCommitPopup--alow-empty",
                },
                status = { recent_commit_count = 20 },
                integrations = { fzf_lua = true },
                signs = {
                    section = { icons.dap.collapsed, icons.dap.expanded },
                    item = { icons.dap.collapsed, icons.dap.expanded },
                },
                commit_popup = { kind = "auto" },
            }
        end,
        keys = { { "<leader>t", "<cmd>Neogit<cr>", desc = "Neogit" } },
    },
}
