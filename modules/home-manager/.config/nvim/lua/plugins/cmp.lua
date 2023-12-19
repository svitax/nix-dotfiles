return {
    { "hrsh7th/cmp-cmdline", event = "CmdlineEnter", dependencies = "hrsh7th/nvim-cmp" },
    { "hrsh7th/cmp-nvim-lsp", event = "InsertEnter", dependencies = { "hrsh7th/nvim-cmp", "neovim/nvim-lspconfig" } },
    { "hrsh7th/cmp-nvim-lsp-signature-help", event = "InsertEnter", dependencies = { "hrsh7th/nvim-cmp" } },
    { "hrsh7th/cmp-path", event = { "CmdlineEnter", "InsertEnter" }, dependencies = { "hrsh7th/nvim-cmp" } },
    { "hrsh7th/cmp-buffer", event = { "CmdlineEnter", "InsertEnter" }, dependencies = "hrsh7th/nvim-cmp" },
    { "rcarriga/cmp-dap", lazy = true, dependencies = { "mfussenegger/nvim-dap", "hrsh7th/nvim-cmp" } },
    { "saadparwaiz1/cmp_luasnip", event = "InsertEnter", dependencies = { "hrsh7th/nvim-cmp", "L3MON4D3/LuaSnip" } },
    { "lukas-reineke/cmp-under-comparator", event = "InsertEnter", dependencies = "hrsh7th/nvim-cmp" },
    {
        "petertriho/cmp-git",
        ft = { "NeogitCommitMessage", "gitcommit", "octo" },
        dependencies = "hrsh7th/nvim-cmp",
        opts = { filetypes = { "NeogitCommitMessage", "gitcommit", "octo" } },
    },
    -- Autocompletion
    {
        "hrsh7th/nvim-cmp",
        lazy = true,
        version = false,
        event = { "InsertEnter", "CmdlineEnter" },
        -- Configure nvim-cmp
        -- See `:help cmp`
        opts = function(_, opts)
            vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })

            local cmp = require("cmp")
            local luasnip = require("luasnip")

            local function t(str)
                return vim.api.nvim_replace_termcodes(str, true, true, true)
            end

            -- Override vim.lsp.util.stylize_markdown to use treesitter
            -- Needs Neovim >= 0.10
            -- vim.lsp.util.stylize_markdown = function(bufnr, contents, opts)
            --   contents = vim.lsp.util._normalize_markdown(contents, {
            --     width = vim.lsp.util._make_floating_popup_size(contents, opts)
            --   })
            --   vim.bo[bufnr].filetype = "markdown"
            --   vim.treesitter.start(bufnr)
            --   vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, contents)
            --
            --   return contents
            -- end
            ---Filter out unwanted entries
            ---@param entry cmp.Entry
            ---@param _ cmp.Context ignored
            ---@return boolean
            local function entry_filter(entry, _)
                return not vim.tbl_contains({
                    "No matches found",
                    "Searching...",
                    "Workspace loading",
                }, entry.completion_item.label)
            end

            local cmp_source_names = {
                buffer = "buffer",
                cmdline = "cmd",
                dap = "dap",
                git = "git",
                luasnip = "snippet",
                nvim_lsp = "lsp",
                nvim_lsp_signature_help = "signature",
                -- obsidian.nvim automatically registers its sources. define source names else my config errors
                -- https://github.com/epwalsh/obsidian.nvim/blob/main/lua/obsidian/init.lua#L161
                -- (find the names of the sources that are automatically registered and add them to this table)
                obsidian = "obsidian",
                obsidian_new = "obsidian",
                obsidian_tags = "obsidian",
                path = "path",
            }

            local compltype_path = {
                dir = true,
                file = true,
                file_in_path = true,
                runtime = true,
            }

            return {
                -- cmp floating window config
                window = {
                    completion = {
                        scrolloff = vim.go.scrolloff,
                        border = "rounded",
                    },
                    documentation = {
                        winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
                        max_width = 80,
                        max_height = 20,
                        border = "rounded",
                    },
                },
                performance = { max_view_entries = 64 },
                experimental = { ghost_text = { hl_group = "CmpGhostText" } },
                sorting = {
                    priority_weight = 2,
                    comparators = {
                        ---@type table[]|function[]
                        comparators = {
                            require("cmp-under-comparator").under,
                            cmp.config.compare.kind,
                            cmp.config.compare.locality,
                            cmp.config.compare.recently_used,
                            cmp.config.compare.exact,
                            cmp.config.compare.score,
                        },
                    },
                },
                -- enable cmp in dap buffers
                enabled = function()
                    return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt" or require("cmp_dap").is_dap_buffer()
                end,
                -- FIXME: shouldn't allow completion in Telescope buffers
                -- enabled = function()
                --   -- disable completion in certain contexts, such as comments
                --   local context = require('cmp.config.context')
                --   -- keep command mode completion enabled when cursor is in a comment
                --   if vim.api.nvim_get_mode().mode == 'c' then
                --     return true
                --   else
                --     return not context.in_treesitter_capture('comment')
                --         and not context.in_syntax_group("Comment")
                --   end
                -- end,
                snippet = {
                    expand = function(args)
                        luasnip.lsp_expand(args.body)
                    end,
                },
                completion = {
                    completeopt = "menu,menuone,noinsert",
                },
                mapping = cmp.mapping.preset.insert({
                    ["<C-c>"] = cmp.mapping.abort(),
                    ["<C-n>"] = {
                        i = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                        c = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
                    },
                    ["<C-p>"] = {
                        i = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                        c = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
                    },
                    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-u>"] = cmp.mapping.scroll_docs(4),
                    ["<CR>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.mapping.close()
                            vim.api.nvim_feedkeys(t("<CR>"), "n", true)
                        else
                            fallback()
                        end
                    end),
                    ["<Tab>"] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                            cmp.confirm()
                        elseif luasnip.expand_or_locally_jumpable() then
                            luasnip.expand_or_jump()
                        else
                            fallback()
                        end
                    end, { "i", "s", "c" }),
                    ["<S-Tab>"] = cmp.mapping(function(fallback)
                        if luasnip.locally_jumpable(-1) then
                            luasnip.jump(-1)
                        else
                            fallback()
                        end
                    end, { "i", "s", "c" }),
                }),
                sources = cmp.config.sources({
                    { name = "luasnip", max_item_count = 3 },
                    { name = "nvim_lsp_signature_help" },
                    {
                        name = "nvim_lsp",
                        max_item_count = 20,
                        entry_filter = entry_filter, -- Suppress LSP completion when workspace is not ready yet
                    },
                    { name = "buffer", keyword_length = 4, max_item_count = 5, group_index = 2 },
                    { name = "path", entry_filter = entry_filter },
                    { name = "git" },
                }),
                cmdline = {
                    sources = {
                        { name = "path", entry_filter = entry_filter },
                        { name = "cmdline", option = { ignore_cmds = {} }, group_index = 2, max_item_count = 30 },
                    },
                    formatting = { max_width = 30 },
                    -- diable cmdline completion for certain commands, such as IncRename
                    enabled = function()
                        -- Set of commands where cmp will be disabled
                        local disabled = {
                            IncRename = true,
                        }
                        -- Get first word of cmdline
                        local cmd = vim.fn.getcmdline():match("%S+")
                        -- Return true if cmd isn't disabled
                        -- else call/return cmp.close(), which returns false
                        return not disabled[cmd] or cmp.close()
                    end,
                },
                cmdline_search = { sources = { { name = "buffer" } } },
                -- Complete vim.ui.input()
                ui_input = {
                    sources = {
                        { name = "path", group_index = 1, entry_filter = entry_filter },
                        { name = "cmdline", group_index = 2, option = { ignore_cmds = {} } },
                        { name = "buffer", group_index = 3 },
                    },
                },
                formatting = {
                    fields = { "abbr", "kind", "menu" },
                    format = function(entry, cmp_item)
                        local icons = require("utils.icons")
                        local compltype = vim.fn.getcmdcompltype()
                        local complpath = compltype_path[compltype]
                        -- Fix cmp path completion not escaping special characters
                        -- (e.g. `#`, spaces) in cmdline,
                        if complpath then
                            local path_escaped = vim.fn.fnameescape(cmp_item.word)
                            cmp_item.word = path_escaped
                            cmp_item.abbr = path_escaped
                        end
                        -- Use special icons for file / directory completions
                        if cmp_item.kind == "File" or cmp_item.kind == "Folder" or complpath then
                            if cmp_item.kind == "Folder" then -- Directories
                                cmp_item.kind = icons.kinds.Folder
                                cmp_item.kind_hl_group = "CmpItemKindFolder"
                            else -- Files
                                local devicons_ok, devicons = pcall(require, "nvim-web-devicons")
                                if devicons_ok then
                                    local icon, icon_hl = devicons.get_icon(
                                        vim.fs.basename(cmp_item.word),
                                        vim.fn.fnamemodify(cmp_item.word, ":e"),
                                        { default = true }
                                    )
                                    cmp_item.kind = icon and icon .. " " or icons.kinds.File
                                    cmp_item.kind_hl_group = icon_hl or "CmpItemKindFile"
                                end
                            end
                        else -- Use special icons for cmdline / calc completions
                            ---@type table<string, string> override icons with `entry.source.name`
                            local icon_override = {
                                cmdline = icons.DotLarge,
                                calc = icons.kinds.Calculator,
                            }
                            cmp_item.kind = icon_override[entry.source.name] or icons.kinds[cmp_item.kind] or ""
                        end

                        if entry.source_name == "nvim_lsp" then
                            cmp_item.menu = "(" .. entry.source.source.client.name .. ")"
                        else
                            cmp_item.menu = "(" .. cmp_source_names[entry.source.name] .. ")"
                        end

                        ---@param field string
                        ---@param min_width integer
                        ---@param max_width integer
                        ---@return nil
                        local function clamp(field, min_width, max_width)
                            if not cmp_item[field] or not type(cmp_item) == "string" then
                                return
                            end
                            -- In case that min_width > max_width
                            if min_width > max_width then
                                min_width, max_width = max_width, min_width
                            end
                            local field_str = cmp_item[field]
                            local field_width = vim.fn.strdisplaywidth(field_str)
                            if field_width > max_width then
                                local former_width = math.floor(max_width * 0.6)
                                local latter_width = math.max(0, max_width - former_width - 1)
                                cmp_item[field] = string.format(
                                    "%s…%s",
                                    field_str:sub(1, former_width),
                                    field_str:sub(-latter_width)
                                )
                            elseif field_width < min_width then
                                cmp_item[field] = string.format("%-" .. min_width .. "s", field_str)
                            end
                        end
                        clamp("abbr", vim.go.pw, math.max(60, math.ceil(vim.o.columns * 0.4)))
                        clamp("menu", 0, math.max(16, math.ceil(vim.o.columns * 0.2)))
                        return cmp_item
                    end,
                },
            }
        end,
        config = function(_, opts)
            for _, source in ipairs(opts.sources) do
                source.group_index = source.group_index or 1
            end

            require("cmp").setup(opts)
            require("cmp").setup.cmdline(":", opts.cmdline)
            require("cmp").setup.cmdline({ "/", "?" }, opts.cmdline_search)
            require("cmp").setup.cmdline({ "@" }, opts.ui_input)
            require("cmp").setup.filetype(
                { "dap-repl", "dapui_watches", "dapui_hover" },
                { sources = { { name = "dap" } } }
            )
        end,
    },
}
