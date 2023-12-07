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
      presets = {
        bottom_search = true,
        long_message_to_split = true,
        inc_rename = false,
        lsp_doc_border = true,
        command_palette = false,
      },
      cmdline = { enabled = false, view = "cmdline" },
      messages = { enabled = false },
      -- cmdline = { enabled = true, view = "cmdline" }, -- i don't like how this replaces my statusline and the hlgs
      -- messages = { enabled = true }, -- this automatically sets cmdheight to 0
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
      views = {
        -- NOTE: if you have cmdheight>0, mini will overlap with the modeline.
        -- offset the default row position for mini (-1) by your cmdheight
        mini = { position = { row = -2 } },
        notify = { merge = true, replace = true },
      },
      routes = {
        {
          -- reroute long notifications to splits
          filter = { event = "notify", min_height = 10 },
          view = "split",
        },
      },
    },
    dependencies = { "MunifTanjim/nui.nvim" },
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
