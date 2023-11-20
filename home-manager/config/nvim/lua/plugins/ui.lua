return {
  -- -- Useful plugin to show you pending keybinds
  -- { 'folke/which-key.nvim',  opts = {} },
  {
    "Cassin01/wf.nvim",
    opts = {},
    config = function(_, opts)
      local which_key = require("wf.builtin.which_key")

      vim.keymap.set(
        "n",
        "<leader>",
        -- mark(opts?: table) -> function
        -- opts?: option
        which_key({text_insert_in_advance = "<Leader>"}),
        { noremap = true, silent = true, nowait = true, desc = "which-key" }
      )
      require("wf").setup(opts)
    end
  }
}
