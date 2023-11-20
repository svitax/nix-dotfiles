return {
  {
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
    opts = {
      -- options for vim.diagnostics.config()
      diagnostics = {
        underline = true,
        update_in_insert = false,
        virtual_text = false,
        -- Enable this for builtin LSP inlay hints on Neovim >= 0.10.0
        -- Be aware you also need to properly configure your LSP server to provide the inlay hints
        inlay_hints = { enable = true },
        -- Add any global capabilities here
        capabilities = {},
      },
      -- Enable the following language servers
      -- Feel free to add/remove any LSPs that you want here.
      -- If you want to override the default filetypes that your language server will attach to you can
      -- define the property 'filetypes' to the map in question
      language_servers = {
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
      }
    },
    config = function(_, opts)
      -- [[ Configure LSP ]]
      --
      -- local plugin = require("lazy.core.config").spec.plugins["neoconf.nvim"]
      -- require("neoconf").setup(require("lazy.core.plugin").values(plugin, "opts", false))
      --
      -- This function gets run when an LSP connects to a particular buffer
      local function on_attach(client, buffer)
        local augroup_highlight = vim.api.nvim_create_augroup("custom-lsp-references", { clear = true })
        local autocmd_clear = vim.api.nvim_clear_autocmds

        -- Setup keymaps
        local opts = { buffer = buffer, remap = false }

        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set('n', '<leader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
        vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, opts)
      end

      -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

      for server, server_config in pairs(opts.language_servers) do
        local config = { capabilities = capabilities, on_attach = on_attach }

        if server_config then
          for k, v in pairs(server_config) do
            config[k] = v
          end
        end

        require('lspconfig')[server].setup(config)
      end

      -- diagnostics
      for name, icon in pairs(require("config").icons.diagnostics) do
        name = "DiagnosticSign" .. name
        vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
      end

      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
    end
  },
}
