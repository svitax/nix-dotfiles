return {
  -- Snippet engine
  {
    "L3MON4D3/LuaSnip",
    dependencies = {
      -- Adds a number of user-friendly snippets
      {
        "rafamadriz/friendly-snippets",
        config = function()
          require("luasnip.loaders.from_vscode").lazy_load()
        end,
      },
    },
    opts = {
      -- Don't store snippet history for less overhead
      history = false,
      -- Event on which to check for exiting a snippet's region
      region_check_events = "InsertEnter",
      delete_check_events = "InsertLeave",
      ft_func = function()
        return vim.split(vim.bo.filetype, ".", { plain = true })
      end,
      config = function(_, opts)
        require("luasnip").setup(opts)
        -- require("luasnip.loaders.from_vscode").lazy_load({ paths = "./snippets" })
        -- require("luasnip.loaders.from_lua").lazy_load({ paths = "./snippets/luasnippets" })
      end,
    },
  },
  -- Autocompletion
  {
    "hrsh7th/nvim-cmp",
    version = false,
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = {
      "FelipeLema/cmp-async-path",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lsp",
      "saadparwaiz1/cmp_luasnip",
      "lukas-reineke/cmp-under-comparator", -- TODO: still using default sorting
      "rcarriga/cmp-dap",
      { "petertriho/cmp-git", opts = { filetypes = { "NeogitCommitMessage", "gitcommit", "octo" } } },
    },
    -- Configure nvim-cmp
    -- See `:help cmp`
    opts = function(_, opts)
      vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })

      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local defaults = require("cmp.config.default")()

      local t = function(str)
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

      local cmp_source_names = {
        async_path = "path",
        buffer = "buffer",
        cmdline = "cmd",
        luasnip = "snippet",
        nvim_lsp = "lsp",
        obsidian = "obsidian",
        dap = "dap",
        git = "git",
      }

      return {
        window = {
          completion = {
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
            col_offset = -3,
            side_padding = 0,
          },
          documentation = {
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
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
          { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "async_path", option = { trailing_slash = true } },
          { name = "buffer", keyword_length = 4, max_item_count = 5, group_index = 2 },
          { name = "git" },
        }),
        cmdline = {
          sources = {
            { name = "cmdline", max_item_count = 30 },
            { name = "async_path", max_item_count = 20 },
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
        cmdline_search = {
          -- view = {
          --   entries = { name = "wildmenu", separator = "|" }
          -- },
          sources = {
            { name = "buffer" },
          },
        },
        formatting = {
          fields = { "kind", "abbr", "menu" },
          format = function(entry, item)
            local icons = require("config").kinds

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
              item.kind = "  "
              item.abbr = item.abbr .. "..."
              item.menu = "(copilot)"
            end

            return item
          end,
        },
        experimental = {
          ghost_text = {
            hl_group = "CmpGhostText",
          },
        },
        sorting = defaults.sorting,
      }
    end,
    config = function(_, opts)
      for _, source in ipairs(opts.sources) do
        source.group_index = source.group_index or 1
      end

      require("cmp").setup(opts)
      require("cmp").setup.cmdline(":", opts.cmdline)
      require("cmp").setup.cmdline({ "/", "?", "@" }, opts.cmdline_search)
      -- stylua: ignore
      require("cmp").setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, { sources = { { name = "dap" }, }, })
    end,
  },
  -- {
  --   "ray-x/lsp_signature.nvim",
  --   opts = {
  --     hi_parameter = "PmenuSel",
  --     hint_enable = false,
  --     -- hint_inline = true, -- need neovim >= 0.10
  --     handler_opts = { border = "none" },
  --   },
  -- },
  -- Autopairing
  {
    "altermo/ultimate-autopair.nvim",
    event = { "InsertEnter", "CmdlineEnter" },
    branch = "v0.6", -- recommended as each new version will have breaking changes
    opts = {
      fastwarp = { map = "<C-l>", cmap = "<C-l>", rmap = "<C-h>", rcmap = "<C-h>" },
      close = { map = "<C-0>", cmap = "<C-0>" }, -- would have liked to map this to <C-)> but i don't know how to make my terminal recognize it as a valid sequence
    },
  },
  -- Smart tab / tabout of pairs
  { "boltlessengineer/smart-tab.nvim", opts = {} },
}
