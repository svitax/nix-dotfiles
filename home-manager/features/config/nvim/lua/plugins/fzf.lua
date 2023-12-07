return {
  {
    "ibhagwan/fzf-lua",
    cmd = { "FzfLua" },
    opts = function()
      local actions = require("fzf-lua").actions
      local e = vim.fn.shellescape

      local function file_edit_or_trouble(selected, opts)
        if #selected > 1 then
          require("fzf-lua").actions.file_sel_to_qf(selected, opts)
          vim.cmd("TroubleToggle quickfix")
        else
          require("fzf-lua").actions.file_edit(selected, opts)
        end
      end

      return {
        winopts = {
          height = 0.40,
          width = 1.00,
          row = 0.87,
          col = 0.00,
          -- border = utils.static.borders.solid,
          preview = {
            default = "builtin",
            vertical = "down:55%",
            horizontal = "right:50%",
            scrollbar = false,
            delay = 32,
            winopts = {
              number = false,
              relativenumber = false,
            },
          },
        },
        hls = {
          normal = "TelescopePromptNormal",
          border = "TelescopePromptBorder",
          title = "TelescopeTitle",
          help_normal = "TelescopeNormal",
          help_border = "TelescopeBorder",
          preview_normal = "TelescopeNormal",
          preview_border = "TelescopeBorder",
          preview_title = "TelescopeTitle",
          -- Builtin preview only
          cursor = "Cursor",
          cursorline = "TelescopePreviewLine",
          cursorlinenr = "TelescopePreviewLine",
          search = "IncSearch",
        },
        fzf_colors = {
          ["fg"] = { "fg", "TelescopeNormal" },
          ["bg"] = { "bg", "TelescopePromptNormal" },
          ["hl"] = { "fg", "TelescopeMatching" },
          ["fg+"] = { "fg", "TelescopeSelection" },
          ["bg+"] = { "bg", "TelescopeSelection" },
          ["hl+"] = { "fg", "TelescopeMatching" },
          ["info"] = { "fg", "TelescopePromptCounter" },
          ["border"] = { "fg", "TelescopeBorder" },
          ["gutter"] = { "bg", "TelescopePromptNormal" },
          ["prompt"] = { "fg", "TelescopePromptPrefix" },
          ["pointer"] = { "fg", "TelescopeSelectionCaret" },
          ["marker"] = { "fg", "TelescopeMultiIcon" },
        },
        fzf_opts = {
          ["--no-scrollbar"] = "",
          ["--no-separator"] = "",
          ["--info"] = e("inline-right"),
          ["--layout"] = e("reverse"),
          ["--marker"] = e("+"),
          ["--pointer"] = e("→"),
          ["--prompt"] = e("/ "),
          ["--border"] = e("none"),
          ["--padding"] = e("0,1"),
          ["--margin"] = e("0"),
          ["--preview-window"] = e("border-sharp"),
        },
        files = { fzf_opts = { ["--info"] = e("inline-right") } },
        grep = {
          rg_opts = table.concat({
            "--hidden",
            "--follow",
            "--smart-case",
            "--column",
            "--line-number",
            "--no-heading",
            "--color=always",
            "-g=!.git/",
            "-e",
          }, " "),
          fzf_opts = { ["--info"] = e("inline-right") },
        },
        lsp = { finder = { fzf_opts = { ["--info"] = e("inline-right") } } },
        defaults = { copen = false }, -- TODO: don't open builtin qf list because we're also opening trouble
        keymap = {
          builtin = {
            ["<C-d>"] = "preview-page-down",
            ["<C-u>"] = "preview-page-up",
          },
          fzf = {
            ["ctrl-c"] = "abort",
            ["ctrl-a"] = "beginning-of-line",
            ["ctrl-e"] = "end-of-line",
            ["ctrl-d"] = "preview-page-down",
            ["ctrl-u"] = "preview-page-up",
            ["ctrl-q"] = "select-all+accept",
          },
        },
        actions = {
          files = {
            ["default"] = file_edit_or_trouble,
            ["ctrl-s"] = actions.file_split,
            ["ctrl-v"] = actions.file_vsplit,
            ["ctrl-t"] = actions.file_tabedit,
            -- ["ctrl-l"] = file_sel_to_trouble_ll,
          },
        },
      }
    end,
    config = function(_, opts)
      require("fzf-lua").setup(opts)
      require("fzf-lua").register_ui_select()

      vim.api.nvim_create_user_command("GoToFile", function()
        require("fzf-lua").files({ cwd = vim.loop.cwd() })
      end, {})
    end,
    keys = {
      {
        "<leader>f",
        "<cmd>FzfLua files cwd=" .. vim.loop.cwd() .. "<cr>",
        desc = "Open file picker at cwd",
      },
      {
        "<leader>/",
        "<cmd>FzfLua live_grep cwd=" .. vim.loop.cwd() .. "<cr>",
        desc = "Open live grep at cwd",
      },
      {
        "<leader>F",
        "<cmd>FzfLua files<cr>",
        desc = "Open file picker",
      },
      {
        "<leader>b",
        "<cmd>FzfLua buffers<cr>",
        desc = "Open buffer picker",
      },
      {
        "<leader>j",
        "<cmd>FzfLua jumps<cr>",
        desc = "Open jumplist picker",
      },
      {
        "<leader>s",
        "<cmd>FzfLua lsp_document_symbols<cr>",
        desc = "Open symbol picker",
      },
      {
        "<leader>S",
        "<cmd>FzfLua lsp_workspace_symbols<cr>",
        desc = "Open workspace symbol picker",
      },
      {
        "<leader>'",
        "<cmd>FzfLua resume<cr>",
        desc = "Open last picker",
      },
    },
  },
}
