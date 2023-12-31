return {
    {
        "neovim/nvim-lspconfig",
        event = "VeryLazy",
        cmd = { "LspInfo", "LspStart" },
        dependencies = {
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
            servers = {},
            -- You can do any additional LSP server setup here
            -- return true if you don't want this server to be setup with lspconfig
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
            -- neoconf
            local plugin = require("lazy.core.config").spec.plugins["neoconf.nvim"]
            require("neoconf").setup(require("lazy.core.plugin").values(plugin, "opts", false))

            -- This function gets run when an LSP connects to a particular buffer
            local function on_attach(client, buffer)
                -- Setup keymaps
                local keymap_opts = { buffer = buffer, noremap = true }

                vim.keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("keep", { desc = "Show docs" }, keymap_opts))
                vim.keymap.set(
                    "n",
                    "gd",
                    vim.lsp.buf.definition,
                    vim.tbl_extend("keep", { desc = "Goto definition" }, keymap_opts)
                )
                vim.keymap.set(
                    "n",
                    "gy",
                    vim.lsp.buf.type_definition,
                    vim.tbl_extend("keep", { desc = "Goto type definition" }, keymap_opts)
                )
                vim.keymap.set(
                    "n",
                    "gr",
                    vim.lsp.buf.references,
                    vim.tbl_extend("keep", { desc = "Goto references" }, keymap_opts)
                )
                vim.keymap.set(
                    "n",
                    "gi",
                    vim.lsp.buf.implementation,
                    vim.tbl_extend("keep", { desc = "Goto implementation" }, keymap_opts)
                )
                vim.keymap.set(
                    "n",
                    "gD",
                    vim.lsp.buf.declaration,
                    vim.tbl_extend("keep", { desc = "Goto declaration" }, keymap_opts)
                )
                vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, keymap_opts)

                vim.api.nvim_create_user_command("InlayHintsToggle", function()
                    vim.lsp.inlay_hint.enable(0, not vim.lsp.inlay_hint.is_enabled())
                end, { desc = "Toggle lsp inlay hints" })
                vim.keymap.set("n", "<leader>i", "<cmd>InlayHintsToggle<cr>", { desc = "Toggle inlay hints" })

                -- vim.keymap.set("n", "gd", "<cmd>TroubleToggle lsp_definitions<cr>", keymap_opts)
                -- vim.keymap.set("n", "gy", "<cmd>TroubleToggle lsp_type_definitions<cr>", keymap_opts)
                -- vim.keymap.set("n", "gr", "<cmd>TroubleToggle lsp_references<cr>", keymap_opts)
                -- vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
                -- vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
                -- vim.keymap.set('n', '<leader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
                -- vim.keymap.set( "n", "<leader>r", vim.lsp.buf.rename, vim.tbl_deep_extend("keep", opts, { desc = "Rename symbol" }))
                -- vim.keymap.set( { "n", "v" }, "<leader>a", vim.lsp.buf.code_action, { desc = "Perform code action", buffer = buffer, remap = true })
                -- vim.keymap.set("n", "K", function()
                --   vim.lsp.buf.format({ async = true })
                -- end, { desc = "Format document", expr = true, buffer = buffer, remap = false })
            end

            require("util").lsp.on_attach(function(client, buffer)
                on_attach(client, buffer)
            end)

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
                local config = { capabilities = capabilities }

                if server_config then
                    for k, v in pairs(server_config) do
                        config[k] = v
                    end
                end

                if opts.setup[server] then
                    if opts.setup[server](server, config) then
                        return
                    end
                elseif opts.setup["*"] then
                    if opts.setup["*"](server, config) then
                        return
                    end
                end

                require("lspconfig")[server].setup(config)
            end

            -- diagnostics
            for name, icon in pairs(require("utils.icons").diagnostics) do
                name = "DiagnosticSign" .. name
                vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
            end

            vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
        end,
    },
}
