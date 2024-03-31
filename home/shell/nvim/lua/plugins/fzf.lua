return {
    {
        "ibhagwan/fzf-lua",
        cmd = { "FzfLua", "GoToFile" },
        opts = function()
            local actions = require("fzf-lua").actions
            local e = vim.fn.shellescape

            local function file_edit_or_qf(selected, opts)
                if #selected > 1 then
                    require("fzf-lua").actions.file_sel_to_qf(selected, opts)
                    require("fzf-lua").quickfix()
                else
                    require("fzf-lua").actions.file_edit(selected, opts)
                end
            end

            return {
                winopts = {
                    height = 0.40,
                    width = 1.00,
                    row = 0.90,
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
                    ["--info"] = "inline-right",
                    ["--layout"] = "reverse",
                    ["--marker"] = "+",
                    ["--pointer"] = "→",
                    ["--prompt"] = "/ ",
                    ["--border"] = "none",
                    ["--padding"] = "0,1",
                    ["--margin"] = "0",
                    ["--preview-window"] = "border-sharp",
                },
                previewers = { git_diff = { pager = "delta --width=$FZF_PREVIEW_COLUMNS" } },
                files = { fzf_opts = { ["--info"] = "inline-right" } },
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
                    fzf_opts = { ["--info"] = "inline-right" },
                },
                lsp = {
                    finder = { fzf_opts = { ["--info"] = "inline-right" } },
                    code_actions = { previewer = "codeaction_native" },
                },
                defaults = { copen = false }, -- TODO: don't open builtin qf list because we're also opening a picker
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
                        ["default"] = file_edit_or_qf,
                        ["ctrl-s"] = actions.file_split,
                        ["ctrl-v"] = actions.file_vsplit,
                        ["ctrl-t"] = actions.file_tabedit,
                        -- ["ctrl-l"] = file_sel_to_ll,
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

            -- override LSP handler functions
            -- stylua: ignore start
            vim.lsp.buf.references = function() require("fzf-lua").lsp_references({ ignore_current_line = true }) end
            vim.lsp.buf.definition = function() require("fzf-lua").lsp_definitions({ jump_to_single_result = true }) end
            vim.lsp.buf.type_definition = function() require("fzf-lua").lsp_typedefs() end
            vim.lsp.buf.implementation = function() require("fzf-lua").lsp_implementations() end
            -- stylua: ignore end
        end,
        keys = {
            {
                "<leader>/",
                "<cmd>FzfLua live_grep_native resume=true cwd=" .. vim.loop.cwd() .. "<cr>",
                desc = "Global search at cwd",
            },
            { "<leader>f", "<cmd>FzfLua files cwd=" .. vim.loop.cwd() .. "<cr>", desc = "Open file picker at cwd" },
            { "<leader>F", "<cmd>FzfLua files<cr>", desc = "Open file picker" },
            { "<leader>b", "<cmd>FzfLua buffers<cr>", desc = "Open buffer picker" },
            { "<leader>B", "<cmd>FzfLua blines<cr>", desc = "Search in current buffer" },
            { "<leader>j", "<cmd>FzfLua jumps<cr>", desc = "Open jumplist picker" },
            { "<leader>s", "<cmd>FzfLua lsp_document_symbols<cr>", desc = "Open symbol picker" },
            { "<leader>S", "<cmd>FzfLua lsp_workspace_symbols<cr>", desc = "Open workspace symbol picker" },
            { "<leader>'", "<cmd>FzfLua resume<cr>", desc = "Open last picker" },
            { "<leader>a", "<cmd>FzfLua lsp_code_actions<cr>", desc = "Perform code action" },
            { "<leader>x", "<cmd>FzfLua quickfix<cr>", desc = "Open quickfix list" },
            { "<leader>?", "<cmd>FzfLua commands<cr>", desc = "Open command palette" },
            { "<C-g>", "<cmd>FzfLua git_status<cr>", desc = "Open git status" },
        },
    },
    {
        -- TODO: make this more like telescope-file-browser
        -- add icons, file permissions, file size, last modified
        -- add preview toggle
        -- if select a file that doesn't exist, create it (<cr>)
        -- BUG: unable to open sometimes when i'm in an oil buffer
        "michel-garcia/fzf-lua-file-browser.nvim",
        dependencies = { "ibhagwan/fzf-lua" },
        opts = {},
        keys = { { "<leader>e", "<cmd>FzfLua file_browser<cr>", desc = "Open file browser" } },
    },
}
