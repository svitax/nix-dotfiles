return {
  -- Adds git related signs to the gutter, as well as utilities for managing changes
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = "▍" },
        change = { text = "▍" },
        delete = { text = "" },
        topdelete = { text = "" },
        changedelete = { text = "▍" },
        untracked = { text = "▍" },
      },
      on_attach = function(buffer)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
        end

        -- stylua: ignore start
        -- map("n", "]h", gs.next_hunk, "Next hunk")
        -- map("n", "[h", gs.prev_hunk, "Previous hunk")
        -- map({ "o", "x" }, "ih", ":<C-u>Gitsigns select_hunk<cr>", "Gitsigns select hunk")
      end,
    },
  },
}
