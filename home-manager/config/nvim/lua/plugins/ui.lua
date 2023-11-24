return {
  -- Useful plugin to show you pending keybinds
  -- { 'folke/which-key.nvim',  opts = {} },
  {
    "Cassin01/wf.nvim",
    opts = {},
    config = function(_, opts)
      local which_key = require("wf.builtin.which_key")
      local leader_key_prefixes = { n = { ["<space>g"] = "debug", ["<space>m"] = "localleader" } }
      local m_key_prefixes = {
        n = {
          ["md"] = "Surround delete",
          ["mr"] = "Surround replace",
          ["ms"] = "Surround add",
          ["mm"] = "Goto matching bracket",
        },
      }

      vim.keymap.set(
        "n",
        "<leader>",
        which_key({
          text_insert_in_advance = "<leader>",
          key_group_dict = leader_key_prefixes["n"],
        }),
        { noremap = true, silent = true, desc = "[wf.nvim] which-key" }
      )

      vim.keymap.set(
        "n",
        "m",
        which_key({
          text_insert_in_advance = "m",
          key_group_dict = m_key_prefixes["n"],
        }),
        { noremap = true, silent = true, desc = "[wf.nvim] m" }
      )

      require("wf").setup(opts)
    end,
  },
  -- {
  --   "stevearc/dressing.nvim",
  --   opts = { select = { telescope = require("telescope.themes").get_ivy({ ... }) }, input = { enabled = false } },
  -- },
}
