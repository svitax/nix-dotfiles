local cmp = require("cmp")
local luasnip = require("luasnip")

vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })

local function t(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- Override vim.lsp.util.stylize_markdown to use treesitter
-- Needs Neovim >= 0.10
vim.lsp.util.stylize_markdown = function(bufnr, contents, opts)
    contents = vim.lsp.util._normalize_markdown(contents, {
        width = vim.lsp.util._make_floating_popup_size(contents, opts),
    })
    vim.bo[bufnr].filetype = "markdown"
    vim.treesitter.start(bufnr)
    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, contents)

    return contents
end

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
    path = "path",
    buffer = "buffer",
    cmdline = "cmd",
    luasnip = "snippet",
    nvim_lsp = "lsp",
    dap = "dap",
    git = "git",
    -- obsidian.nvim automatically registers its sources. define source names else my config errors
    -- https://github.com/epwalsh/obsidian.nvim/blob/main/lua/obsidian/init.lua#L161
    -- (find the names of the sources that are automatically registered and add them to this table)
    obsidian = "obsidian",
    obsidian_new = "obsidian",
    obsidian_tags = "obsidian",
}

cmp.setup({
    performance = { max_view_entries = 64 },
    experimental = { ghost_text = { hl_group = "CmpGhostText" } },
    -- cmp floating window config
    window = {
        completion = {
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
            col_offset = -3,
            side_padding = 0,
        },
        documentation = {
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
            max_width = 80,
            max_height = 20,
        },
    },
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
    completion = {
        completeopt = "menu,menuone,noinsert",
        -- autocomplete = false,
    },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body) -- For `luasnip` users.
        end,
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
        {
            name = "nvim_lsp",
            max_item_count = 20,
            entry_filter = entry_filter, -- Suppress LSP completion when workspace is not ready yet
        },
        { name = "luasnip", max_item_count = 3 },
        { name = "buffer", keyword_length = 4, max_item_count = 5, group_index = 2 },
        { name = "path", entry_filter = entry_filter },
        { name = "git" },
    }),
    -- enable cmp in dap buffers
    enabled = function()
        return vim.bo[0].buftype ~= "prompt"
        -- or require("cmp_dap").is_dap_buffer()
    end,
    formatting = {
        fields = { "kind", "abbr", "menu" },
        format = function(entry, item)
            local icons = require("fennec.icons").kinds

            if icons[item.kind] then
                item.kind = " " .. icons[item.kind] .. ""
                item.menu = "(" .. cmp_source_names[entry.source.name] .. ")"
            end

            if entry.source_name == "nvim_lsp" then
                item.menu = "(" .. entry.source.source.client.name .. ")"
            end

            if entry.source.name == "git" then
                item.kind = "  "
            end

            if entry.source.name == "copilot" then
                item.abbr = item.abbr .. "..."
                item.menu = "(copilot)"
            end

            return item
        end,
    },
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ "/", "?" }, {
    sources = { { name = "buffer" } },
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(":", {
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
})

cmp.setup.cmdline({ "@" }, {
    sources = {
        { name = "path", group_index = 1, entry_filter = entry_filter },
        { name = "cmdline", group_index = 2, option = { ignore_cmds = {} } },
        { name = "buffer", group_index = 3 },
    },
})
