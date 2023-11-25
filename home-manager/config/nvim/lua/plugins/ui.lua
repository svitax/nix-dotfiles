return {
  -- Useful plugin to show you pending keybinds
  -- { 'folke/which-key.nvim',  opts = {} },
  {
    "Cassin01/wf.nvim",
    opts = {},
    config = function(_, opts)
      local which_key = require("wf.builtin.which_key")
      local leader_key_prefixes = { n = { ["<space>g"] = "debug", ["<space>m"] = "localleader" } }
      local m_key_prefixes = {}

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
  {
    "shellRaining/hlchunk.nvim",
    event = { "UIEnter" },
    opts = function()
      local palette = require("nightfox.palette").load("gruvfox")
      return {
        exclude_filetypes = {
          NeogitStatus = true,
          NeogitCommitMessage = true,
          NeogitConsole = true,
        },
        chunk = { style = palette.magenta.base, notify = false },
        indent = { enable = false },
        line_num = { enable = false },
        blank = { enable = false },
      }
    end,
  },
}
