return {
  {
    "is0n/fm-nvim",
    opts = { ui = { float = { border = "" }, mappings = { q = ":q<CR>" } } },
    keys = {
      {
        "<leader>e",
        "<cmd>Lf %<cr>",
        desc = "File explorer",
      },
    },
  },
}
