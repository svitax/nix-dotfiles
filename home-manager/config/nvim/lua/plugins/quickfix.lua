return {
  {
    "folke/trouble.nvim",
    opts = { use_diagnostic_signs = true, mode = "quickfix" },
    cmd = { "Trouble", "TroubleToggle" },
    keys = {
      { "<leader>d", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Open diagnostics" },
      { "<leader>D", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Open workspace diagnostics" },
      { "<leader>x", "<cmd>TroubleToggle quickfix<cr>", desc = "Open quickfix list" },
    },
  },
}
