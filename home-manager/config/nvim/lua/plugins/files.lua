return {
  {
    "lmburns/lf.nvim",
    dependencies = { "akinsho/toggleterm.nvim" },
    -- init = function()
    --   vim.g.lf_netrw = 1
    -- end,
    opts = {
      winblend = 0,
      escape_quit = false,
      border = "",
    },
    config = function(_, opts)
      require("lf").setup(opts)
      vim.keymap.set("n", "<leader>e", "<cmd>Lf<cr>", { desc = "File explorer" })
    end,
  },
}
