local function flash(prompt_bufnr)
  require("flash").jump({
    pattern = "^",
    label = { after = { 0, 0 } },
    search = {
      mode = "search",
      exclude = {
        function(win)
          return vim.bo[vim.api.nvim_win_get_buf(win)].filetype ~= "TelescopeResults"
        end,
      },
    },
    action = function(match)
      local picker = require("telescope.actions.state").get_current_picker(prompt_bufnr)
      picker:set_selection(match.pos[1] - 1)
    end,
  })
end

return {
  -- Fuzzy finder (files, lsp, etc)
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- Fuzzy finder algorithm
      { "natecraddock/telescope-zf-native.nvim" },
    },
    opts = {
      defaults = {
        layout_strategy = "bottom_pane",
        layout_config = {
          height = 25,
        },
        border = true,
        borderchars = {
          prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
          results = { " " },
          preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        },
        sorting_strategy = "ascending",
        mappings = {
          n = {
            s = flash,
            ["<c-c>"] = "close",
          },
          i = {
            ["<c-j>"] = "move_selection_next",
            ["<c-k>"] = "move_selection_previous",
            ["<c-f>"] = "to_fuzzy_refine",
            ["<c-c>"] = "close",
            -- ["<c-o>"] = "preview_scrolling_left",
            -- ["<c-i>"] = "preview_scrolling_right",
            ["<c-;>"] = "cycle_previewers_next",
            ["<c-,>"] = "cycle_previewers_prev",
          },
        },
      },
    },
    config = function(_, opts)
      local telescope = require("telescope")
      telescope.setup(opts)
      telescope.load_extension("zf-native")

      vim.api.nvim_create_user_command("GoToFile", function()
        require("telescope.builtin")["find_files"]()
      end, {})
    end,
    keys = {
      {
        "<leader>f",
        require("util").telescope("files", { cwd = vim.fn.expand("%:h") }),
        desc = "Open file picker at cwd",
      },
      {
        "<leader>F",
        require("util").telescope("files", {}),
        desc = "Open file picker",
      },
      {
        "<leader>b",
        require("util").telescope("buffers", { initial_mode = "normal", sort_lastused = true, sort_mru = true }),
        desc = "Open buffer picker",
      },
      {
        "<leader>j",
        require("util").telescope("jumplist"),
        desc = "Open jumplist picker",
      },
      {
        "<leader>s",
        require("util").telescope("lsp_document_symbols"),
        desc = "Open symbol picker",
      },
      {
        "<leader>S",
        require("util").telescope("lsp_workspace_symbols"),
        desc = "Open workspace symbol picker",
      },
      {
        "<leader>d",
        require("util").telescope("diagnostics", { bufnr = 0 }),
        desc = "Open diagnostics picker",
      },
      {
        "<leader>D",
        require("util").telescope("diagnostics"),
        desc = "Open workspace diagnostics picker",
      },
      {
        "<leader>'",
        require("util").telescope("resume"),
        desc = "Open last picker",
      },
    },
  },
}
