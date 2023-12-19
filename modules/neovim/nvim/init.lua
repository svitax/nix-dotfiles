local cmd = vim.cmd
local fn = vim.fn
local opt = vim.opt
local g = vim.g

g.mapleader = " "
g.maplocalleader = "\\"

-- Configure Neovim diagnostic messages

local function prefix_diagnostic(prefix, diagnostic)
    return string.format(prefix .. " %s", diagnostic.message)
end

local sign = function(opts)
    fn.sign_define(opts.name, {
        texthl = opts.name,
        text = opts.text,
        numhl = "",
    })
end
-- Requires Nerd fonts
sign({ name = "DiagnosticSignError", text = "󰅚" })
sign({ name = "DiagnosticSignWarn", text = "⚠" })
sign({ name = "DiagnosticSignInfo", text = "ⓘ" })
sign({ name = "DiagnosticSignHint", text = "󰌶" })

vim.diagnostic.config({
    virtual_text = {
        prefix = "",
        format = function(diagnostic)
            local severity = diagnostic.severity
            if severity == vim.diagnostic.severity.ERROR then
                return prefix_diagnostic("󰅚", diagnostic)
            end
            if severity == vim.diagnostic.severity.WARN then
                return prefix_diagnostic("⚠", diagnostic)
            end
            if severity == vim.diagnostic.severity.INFO then
                return prefix_diagnostic("ⓘ", diagnostic)
            end
            if severity == vim.diagnostic.severity.HINT then
                return prefix_diagnostic("󰌶", diagnostic)
            end
            return prefix_diagnostic("■", diagnostic)
        end,
    },
    signs = true,
    update_in_insert = false,
    underline = true,
    severity_sort = true,
    float = {
        focusable = false,
        style = "minimal",
        border = "rounded",
        source = "always",
        header = "",
        prefix = "",
    },
})

-- Native plugins
cmd.filetype("plugin", "indent", "on")
cmd.packadd("cfilter") -- Allows filtering the quickfix list with :cfdo

-- let sqlite.lua (which some plugins depend on) know where to find sqlite
vim.g.sqlite_clib_path = require("luv").os_getenv("LIBSQLITE")
