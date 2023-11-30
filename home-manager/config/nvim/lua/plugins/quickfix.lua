return {
  {
    "folke/trouble.nvim",
    opts = {
      use_diagnostic_signs = true,
      mode = "quickfix",
      indent_lines = false,
      action_keys = { toggle_fold = { "za", "<S-tab>" } },
    },
    cmd = { "Trouble", "TroubleToggle" },
    keys = {
      { "<leader>d", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Open diagnostics" },
      { "<leader>D", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Open workspace diagnostics" },
      { "<leader>x", "<cmd>TroubleToggle quickfix<cr>", desc = "Open quickfix list" },
    },
  },
}
