return {
    {
        "folke/trouble.nvim",
        opts = {
            use_diagnostic_signs = true,
            padding = false,
            mode = "quickfix",
            indent_lines = false,
            action_keys = { toggle_fold = { "za", "<S-tab>" } },
        },
        cmd = { "Trouble", "TroubleToggle" },
        keys = {
            { "<leader>d", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Open diagnostics" },
            { "<leader>D", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Open workspace diagnostics" },
        },
    },
}
