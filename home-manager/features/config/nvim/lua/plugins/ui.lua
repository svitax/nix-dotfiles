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
        { "n", "x" },
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
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      messages = { enabled = false },
      cmdline = { enabled = true, view = "cmdline" },
      notify = { enabled = true, view = "notify" },
      presets = {
        bottom_search = true,
        long_message_to_split = true,
        inc_rename = false,
        lsp_doc_border = true,
        command_palette = false,
      },
      popupmenu = { backend = "cmp" },
      lsp = {
        progress = { enabled = false }, -- NOTE: fidget.nvim behaves and looks nicer
        signature = { enabled = false, auto_open = { enabled = false } },
        documentation = { view = "messages" },
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
          ["cmp.entry.get_documentation"] = true,
        },
      },
    },
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
  },
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
