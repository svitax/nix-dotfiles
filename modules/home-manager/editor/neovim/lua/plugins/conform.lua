return {
    {
        "stevearc/conform.nvim",
        event = { "BufWritePre", "LspAttach" },
        cmd = { "ConformInfo" },
        init = function()
            vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
        end,
        opts = function()
            -- Automatically run slow formatters async
            -- This will detect which formatters take too long to run synchronously and will run them async on save instead
            local slow_format_filetypes = {}
            return {
                notify_on_error = false,
                format_on_save = function(bufnr)
                    -- Disable autoformatting with a global or buffer-local variable
                    if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
                        return
                    end

                    if slow_format_filetypes[vim.bo[bufnr].filetype] then
                        return
                    end
                    local function on_format(err)
                        if err and err:match("timeout$") then
                            slow_format_filetypes[vim.bo[bufnr].filetype] = true
                        end
                    end

                    return { timeout_ms = 200, lsp_fallback = true }, on_format
                end,
                format_after_save = function(bufnr)
                    if not slow_format_filetypes[vim.bo[bufnr].filetype] then
                        return
                    end
                    return { lsp_fallback = true }
                end,
            }
        end,
        config = function(_, opts)
            require("conform").setup(opts)

            -- Commands to toggle autoformat on save
            vim.api.nvim_create_user_command("FormatDisable", function(args)
                if args.bang then
                    -- FormatDisable! will disable formatting just for this buffer
                    vim.b.disable_autoformat = true
                else
                    vim.g.disable_autoformat = true
                end
            end, { desc = "Disable autoformat on save", bang = true })
            vim.api.nvim_create_user_command("FormatEnable", function()
                vim.b.disable_autoformat = false
                vim.g.disable_autoformat = false
            end, { desc = "Re-enable autoformat on save" })
            vim.api.nvim_create_user_command("FormatToggle", function(args)
                if args.bang then
                    -- FormatToggle! will toggle formatting just for this buffer
                    vim.b.disable_autoformat = not vim.b.disable_autoformat
                else
                    vim.g.disable_autoformat = not vim.g.disable_autoformat
                end
            end, { desc = "Toggle autoformat on save", bang = true })

            -- Command to run async formatting
            vim.api.nvim_create_user_command("Format", function(args)
                local range = nil
                if args.count ~= -1 then
                    local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
                    range = {
                        start = { args.line1, 0 },
                        ["end"] = { args.line2, end_line:len() },
                    }
                end
                require("conform").format({ async = true, lsp_fallback = true, range = range })
            end, { range = true })
        end,
        keys = {
            {
                "<leader>k",
                "<cmd>Format<cr>",
                mode = "",
                desc = "Format document",
            },
            { "<leader>z", "<cmd>FormatToggle<cr>", desc = "Toggle format on save" },
        },
    },
}