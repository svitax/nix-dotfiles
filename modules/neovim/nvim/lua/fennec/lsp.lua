---@mod user.lsp
---
---@brief [[
---LSP related functions
---@brief ]]

local M = {}

---Gets a 'ClientCapabilities' object, describing the LSP client capabilities
---Extends the object with capabilities provided by plugins.
---@return lsp.ClientCapabilities
function M.make_client_capabilities()
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    -- Add com_nvim_lsp capabilities
    local cmp_lsp = require("cmp_nvim_lsp")
    local cmp_lsp_capabilities = cmp_lsp.default_capabilities()
    capabilities = vim.tbl_deep_extend("keep", capabilities, cmp_lsp_capabilities)
    -- Add any additional plugin capabilities here.
    -- Make sure to follow the instructions provided in the plugin's docs.

    -- Neovim hasn't added foldingRange to default capabilities,
    -- Users must add it manually to let server know
    -- Needed for nvim-ufo functionality through lsp client
    capabilities.textDocument.foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
    }
    return capabilities
end

function M.on_attach(client, buffer)
    -- Setup keymaps
    local keymap_opts = { buffer = buffer, noremap = true }

    -- stylua: ignore
    vim.keymap.set( "n", "<leader>k", vim.lsp.buf.hover, { buffer = buffer, noremap = true, desc = "Show docs" })
    vim.keymap.set("n", "gd", "<cmd>TroubleToggle lsp_definitions<cr>", keymap_opts)
    vim.keymap.set("n", "gy", "<cmd>TroubleToggle lsp_type_definitions<cr>", keymap_opts)
    vim.keymap.set("n", "gr", "<cmd>TroubleToggle lsp_references<cr>", keymap_opts)
    vim.keymap.set("n", "gi", vim.lsp.buf.implementation, keymap_opts)
    vim.keymap.set("n", "gD", vim.lsp.buf.declaration, keymap_opts)
    vim.keymap.set("i", "<C-s>", vim.lsp.buf.signature_help, keymap_opts)
    -- vim.keymap.set("n", "gd", vim.lsp.buf.definition, keymap_opts)
    -- vim.keymap.set("n", "gy", vim.lsp.buf.type_definition, keymap_opts)
    -- vim.keymap.set("n", "gr", vim.lsp.buf.references, keymap_opts)
    -- vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
    -- vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
    -- vim.keymap.set('n', '<leader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
    -- vim.keymap.set( "n", "<leader>r", vim.lsp.buf.rename, vim.tbl_deep_extend("keep", opts, { desc = "Rename symbol" }))
    -- vim.keymap.set( { "n", "v" }, "<leader>a", vim.lsp.buf.code_action, { desc = "Perform code action", buffer = buffer, remap = true })
    -- vim.keymap.set("n", "<leader>k", function()
    --   vim.lsp.buf.format({ async = true })
    -- end, { desc = "Format document", expr = true, buffer = buffer, remap = false })
end

M.define_diagnostics = function()
    -- diagnostics
    for name, icon in pairs(require("fennec.icons").diagnostics) do
        name = "DiagnosticSign" .. name
        vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
    end
end

return M
