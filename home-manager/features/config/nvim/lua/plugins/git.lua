local gitsigns_icons = require("config").icons.git.signs
return {
  -- Adds git related signs to the gutter, as well as utilities for managing changes
  {
    "lewis6991/gitsigns.nvim",
    event = "BufReadPre",
    dependencies = "nvim-lua/plenary.nvim",
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = gitsigns_icons.add },
        change = { text = gitsigns_icons.change },
        delete = { text = gitsigns_icons.delete },
        topdelete = { text = gitsigns_icons.topdelete },
        changedelete = { text = gitsigns_icons.changedelete },
        untracked = { text = gitsigns_icons.untracked },
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
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    opts = function()
      local icons = require("config").icons

      return {
        console_timeout = 10000,
        auto_show_console = true,
        disable_hint = true,
        disable_commit_confirmation = true,
        disable_insert_on_commit = true,
        kind = "tab",
        use_per_project_settings = true,
        remember_settings = true,
        ignored_settings = {
          "NeogitPushPopup--force-with-lease",
          "NeogitPushPopup--force",
          "NeogitCommitPopup--alow-empty",
        },
        integrations = {
          telescope = true,
        },
        signs = {
          section = { icons.dap.collapsed, icons.dap.expanded },
          item = { icons.dap.collapsed, icons.dap.expanded },
        },
        commit_popup = { kind = "auto" },
        use_telescope = true,
      }
    end,
    keys = { { "<leader>t", "<cmd>Neogit<cr>", desc = "Neogit" } },
  },
}
