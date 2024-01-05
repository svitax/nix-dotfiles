return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                ruff_lsp = {},
                pyright = {},
                -- pylyzer doesn't support local package resolve (e.g. doesn't recognize packages in .venv)
                -- https://github.com/mtshiba/pylyzer/issues/22
                -- look into using once this issue is resolved
                -- pylyzer = {},
            },
        },
    },
    -- { "mfussenegger/nvim-lint", opts = { linters_by_ft = { python = { "mypy" } } } },
    {
        "klafyvel/vim-slime-cells",
        dependencies = { "jpalardy/vim-slime", "anuvyklack/hydra.nvim" },
        ft = { "python" },
        init = function()
            -- vim.g.slime_target = "zellij"
            -- vim.g.slime_default_config = { session_id = "current", relative_pane = "right", relative_move_back = "left" }
            vim.g.slime_target = "tmux"
            vim.g.slime_default_config = { socket_name = "default", target_pane = "{right}" }
            vim.g.slime_dont_ask_default = 1
            vim.g.slime_bracketed_paste = 1
            vim.g.slime_no_mappings = 1
            vim.g.slime_cell_delimiter = "^\\s*# %%"
            vim.g.slime_cells_no_highlight = 1
        end,
        config = function()
            local hydra = require("hydra")

            local function keys(str)
                return function()
                    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(str, true, false, true), "m", true)
                end
            end

            hydra({
                name = "vim-slime-cells navigator",
                config = {
                    color = "pink",
                    invoke_on_body = true,
                    hint = {
                        border = "rounded",
                    },
                },
                mode = { "n" },
                body = "<leader>E", -- this is the key that triggers the hydra
                heads = {
                    { "j", keys("]b"), { desc = "down" } },
                    { "k", keys("[b"), { desc = "up" } },
                    { "r", "<Plug>SlimeCellsSendAndGoToNext", { desc = "evaluate cell" } },
                    { "o", keys("/# %%<cr>:nohl<cr>O# %%<cr>"), { desc = "new cell down", exit = true } },
                    { "O", keys("?# %%<cr>:nohl<cr>O# %%<cr>"), { desc = "new cell up", exit = true } },
                    { "<esc>", nil, { exit = true } },
                    { "q", nil, { exit = true } },
                },
            })
            vim.keymap.set("n", "]b", "<Plug>SlimeCellsNext", { desc = "Next cell" })
            vim.keymap.set("n", "[b", "<Plug>SlimeCellsPrev", { desc = "Previous cell" })
            vim.keymap.set("n", "<leader>e", "<Plug>SlimeCellsSendAndGoToNext", { desc = "Evluate cell" })
            vim.keymap.set({ "x", "o" }, "<leader>e", "<Plug>SlimeRegionSend", { desc = "Evaluate region" })
        end,
    },
    -- { "gabenespoli/vim-jupycent" },
    -- {
    --   "benlubas/molten-nvim",
    --   event = "VeryLazy",
    --   dependencies = { "3rd/image.nvim" },
    --   build = ":UpdateRemotePlugins",
    --   init = function()
    --     vim.g.molten_auto_open_output = false
    --     vim.g.molten_image_provider = "image.nvim"
    --     vim.g.molten_output_crop_border = true
    --     vim.g.molten_output_win_border = { "", "━", "", "" }
    --     vim.g.molten_output_win_max_height = 10
    --     vim.g.molten_virt_text_output = true
    --     vim.g.molten_use_border_highlights = true
    --     vim.g.molten_virt_lines_off_by_1 = true
    --     vim.g.molten_wrap_output = true
    --
    --     vim.keymap.set("n", "<localleader>mi", "<cmd>MoltenInit<cr>", { desc = "initialize Molten", silent = true })
    --     vim.keymap.set("n", "<localleader>mp", function()
    --       local venv = os.getenv("VIRTUAL_ENV")
    --       if venv ~= nil then
    --         venv = string.match(venv, "/(.+)$")
    --         vim.cmd(("MoltenInit %s"):format(venv))
    --       else
    --         vim.cmd("MoltenInit python3")
    --       end
    --     end, { desc = "Initialize Molden for python3", silent = true, noremap = true })
    --     vim.keymap.set(
    --       "n",
    --       "<localleader>me",
    --       "<cmd>MoltenEvaluateOperator<cr>",
    --       { desc = "evaluate operator", silent = true }
    --     )
    --     vim.keymap.set(
    --       "n",
    --       "<localleader>mr",
    --       "<cmd>MoltenReevaluateCell<cr>",
    --       { desc = "reevaluate cell", silent = true }
    --     )
    --     vim.keymap.set(
    --       { "x", "o" },
    --       "<leader>me",
    --       ":<C-u>MoltenEvaluateVisual<cr>gv",
    --       { desc = "evaluate visual selection" }
    --     )
    --     vim.keymap.set(
    --       "n",
    --       "<localleader>mo",
    --       "<cmd>noautocmd MoltenEnterOutput<cr>",
    --       { desc = "open output window", silent = true }
    --     )
    --     vim.keymap.set(
    --       "n",
    --       "<localleader>mh",
    --       "<cmd>MoltenHideOutput<cr>",
    --       { desc = "close output window", silent = true }
    --     )
    --   end,
    -- },
    -- {
    --   "GCBallesteros/NotebookNavigator.nvim",
    --   dependencies = { "echasnovski/mini.ai", "Vigemus/iron.nvim", "anuvyklack/hydra.nvim" },
    --   config = function()
    --     local nn = require("notebook-navigator")
    --     local ai = require("mini.ai")
    --     nn.setup({ activate_hydra_keys = "<localleader>h" })
    --     ai.setup({ custom_textobjects = { h = nn.miniai_spec } })
    --   end,
    -- },
    -- {
    --   "Vigemus/iron.nvim",
    --   main = "iron.core",
    --   opts = function()
    --     return { config = { repl_open_cmd = require("iron.view").split("40%") } }
    --   end,
    -- },
    -- {
    --   -- BUG: kitty has builtin support for tmux with passthrough mode, but doesn't work if using zellij,
    --   -- Unfortunately zellij doesn't support something similar.
    --   -- https://github.com/zellij-org/issues/775 and https://github.com/kovidgoyal/kitty/issues/5900#issuecomment-1387420845
    --   "3rd/image.nvim",
    --   opts = {
    --     backend = "kitty",
    --     integrations = {
    --       markdown = {
    --         enabled = true,
    --         clear_in_insert_mode = false,
    --         download_remote_images = true,
    --         only_render_image_at_cursor = false,
    --         filetypes = { "markdown", "quarto" },
    --       },
    --     },
    --     max_width = 100,
    --     max_height = 12,
    --     max_height_window_percentage = math.huge,
    --     window_overlap_clear_enabled = true,
    --     window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
    --   },
    -- },
    -- {
    --   "quarto-dev/quarto-nvim",
    --   dependencies = {
    --     "jmbuhr/otter.nvim",
    --     "anuvyklack/hydra.nvim",
    --   },
    --   ft = { "quarto", "markdown" },
    --   config = function()
    --     local map = vim.keymap.set
    --
    --     map("n", "<localleader>P", quarto.quartoPreview, { desc = "Preview Quarto doc", silent = true, noremap = true })
    --     map(
    --       "n",
    --       "<localleader>c",
    --       "i```{}\r```<up><right>",
    --       { desc = "Create new code cell", silent = true, noremap = true }
    --     )
    --
    --     local hydra = require("hydra")
    --     hydra({
    --       name = "QuartoNavigator",
    --       hint = false,
    --       config = {
    --         color = "pink",
    --         invoke_on_body = true,
    --         hint = false,
    --       },
    --       mode = { "n" },
    --       body = "localleader>j",
    --       heads = {
    --         { "j", keys("]b"), { desc = "down", remap = true, noremap = false } },
    --         { "k", keys("[b"), { desc = "up", remap = true, noremap = false } },
    --         { "o", keys("/```<cr>:nohl<cr>o<cr>`<c-j>"), { desc = "new cell down", exit = true } },
    --         { "O", keys("?```<cr>:nohl<cr>o<cr>`<c-j>"), { desc = "new cell up", exit = true } },
    --         { "l", "QuartoSend<cr>", { desc = "run" } },
    --         { "s", ":noautocmd MoltenEnterOutput<cr>", { desc = "show" } },
    --         { "h", ":QuartoHideOutput", { desc = "hide" } },
    --         { "a", ":QuartoSendAbove<cr>", { desc = "run above" } },
    --         { "<esc>", nil, { exit = true } },
    --         { "q", nil, { exit = true } },
    --       },
    --     })
    --   end,
    -- },
}
