return {
  {
    -- Incremental LSP renaming based on Neovim's command-preview feature
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    opts = {},
    keys = {
      {
        "<leader>r",
        function()
          return ":IncRename " .. vim.fn.expand("<cword>")
        end,
        expr = true,
        desc = "Rename symbol",
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { "j-hui/fidget.nvim", opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      { "folke/neodev.nvim", opts = {} },
      { "folke/neoconf.nvim", cmd = "Neoconf", config = false, dependencies = { "nvim-lspconfig" } },
    },
    opts = {
      -- options for vim.diagnostics.config()
      diagnostics = {
        underline = true,
        update_in_insert = false,
        virtual_text = false,
        severity_sort = true,
      },
      -- Enable this for builtin LSP inlay hints on Neovim >= 0.10.0
      -- Be aware you also need to properly configure your LSP server to provide the inlay hints
      -- FIXME: not setup in config function yet
      inlay_hints = { enabled = true },
      -- Add any global capabilities here
      -- FIXME: not setup in config function yet
      capabilities = {},
      -- Enable the following language servers
      -- Feel free to add/remove any LSPs that you want here.
      -- If you want to override the default filetypes that your language server will attach to you can
      -- define the property 'filetypes' to the map in question
      servers = {
        lua_ls = {},
        nixd = {},
        pyright = {},
        yamlls = {},
        taplo = {},
      },
      -- You can do any additional LSP server setup here
      -- return true if you don't want this server to be setup with lspconfig
      -- TODO: not set up in config function yet
      setup = {
        -- -- example to setup with typescript.nvim
        -- tsserver = function (_, opts)
        --   require("typescript").setup({server = opts})
        --   return true
        -- end,
        -- -- Specify * to use this function as a fallback for any server
        -- ["*"] = function (server, opts) end
      },
    },
    config = function(_, opts)
      -- [[ Configure LSP ]]
      local plugin = require("lazy.core.config").spec.plugins["neoconf.nvim"]
      require("neoconf").setup(require("lazy.core.plugin").values(plugin, "opts", false))

      -- This function gets run when an LSP connects to a particular buffer
      local function on_attach(client, buffer)
        local augroup_highlight = vim.api.nvim_create_augroup("custom-lsp-references", { clear = true })
        local autocmd_clear = vim.api.nvim_clear_autocmds

        -- Setup keymaps
        local keymap_opts = { buffer = buffer, remap = false }

        vim.keymap.set("n", "K", vim.lsp.buf.hover, keymap_opts)
        vim.keymap.set("n", "gd", vim.lsp.buf.definition, keymap_opts)
        vim.keymap.set("n", "gy", vim.lsp.buf.type_definition, opts)
        vim.keymap.set("n", "gr", vim.lsp.buf.references, keymap_opts)
        vim.keymap.set("n", "gi", vim.lsp.buf.implementation, keymap_opts)
        vim.keymap.set("n", "gD", vim.lsp.buf.declaration, keymap_opts)
        vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, keymap_opts)
        -- vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
        -- vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
        -- vim.keymap.set('n', '<leader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
        -- vim.keymap.set( "n", "<leader>r", vim.lsp.buf.rename, vim.tbl_deep_extend("keep", opts, { desc = "Rename symbol" }))
        -- vim.keymap.set( { "n", "v" }, "<leader>a", vim.lsp.buf.code_action, { desc = "Perform code action", buffer = buffer, remap = true })
        -- vim.keymap.set("n", "<leader>k", function()
        --   vim.lsp.buf.format({ async = true })
        -- end, { desc = "Format document", expr = true, buffer = buffer, remap = false })
      end

      -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
      -- local capabilities = vim.lsp.protocol.make_client_capabilities()
      -- capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        require("cmp_nvim_lsp").default_capabilities(),
        opts.capabilities
      )

      -- idk what these textdocument actually capabilities do
      capabilities.textDocument.completion.completionItem = {
        documentationFormat = { "markdown", "plaintext" },
        snippetSupport = true,
        preselectSupport = true,
        insertReplaceSupport = true,
        labelDetailsSupport = true,
        deprecatedSupport = true,
        commitCharacterSupport = true,
        tagSupport = { valueSet = { 1 } },
        resolveSupport = {
          properties = {
            "documentation",
            "detail",
            "additionalTextEdits",
          },
        },
      }
      -- Neovim hasn't added foldingRange to default capabilities,
      -- Users must add it manually to let server know
      -- Needed for nvim-ufo functionality through lsp client
      capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      }

      for server, server_config in pairs(opts.servers) do
        local config = { capabilities = capabilities, on_attach = on_attach }

        if server_config then
          for k, v in pairs(server_config) do
            config[k] = v
          end
        end

        require("lspconfig")[server].setup(config)
      end

      -- diagnostics
      for name, icon in pairs(require("config").icons.diagnostics) do
        name = "DiagnosticSign" .. name
        vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
      end

      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
    end,
  },
  {
    "luckasRanarison/clear-action.nvim",
    event = "LspAttach",
    keys = { { "<leader>a", "<cmd>lua require('clear-action').code_action()<cr>", desc = "Perform code action" } },
    opts = {
      popup = { highlights = { title = "NormalFloat" } },
      signs = {
        combine = true,
        icons = {
          quickfix = "  ",
          refactor = "  ",
          source = "  ",
          combined = "  ",
        },
      },
    },
  },
  {
    -- BUG: virtual_lines = { highlight_whole_line = false } gets reset whenever you do require("lsp_lines").toggle()
    -- so we can't have lsp_lines disabled by default and have highlight_whole_line = false at the same time
    -- This bug was introduced here https://git.sr.ht/~whynothugo/lsp_lines.nvim/commit/512d8c79637e7aeb889240c2e0ca8ae327940737
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    commit = "84578d4a6c91d7db25e1a47727f5b04177a77011",
    event = "LspAttach",
    config = function(_, opts)
      vim.diagnostic.config({ virtual_lines = false })
      require("lsp_lines").setup()
    end,
    keys = {
      {
        "<leader>l",
        function()
          require("lsp_lines").toggle()
        end,
        desc = "Toggle virtual diagnostic lines",
      },
    },
  },
}
