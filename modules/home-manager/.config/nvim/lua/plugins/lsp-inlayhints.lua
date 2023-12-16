return {
    {
        "lvimuser/lsp-inlayhints.nvim",
        branch = "anticonceal",
        opts = {},
        config = function(_, opts)
            require("lsp-inlayhints").setup(opts)

            vim.api.nvim_create_augroup("LspAttach_inlayhints", {})
            vim.api.nvim_create_autocmd("LspAttach", {
                group = "LspAttach_inlayhints",
                callback = function(args)
                    if not (args.data and args.data.client_id) then
                        return
                    end

                    local bufnr = args.buf
                    local client = vim.lsp.get_client_by_id(args.data.client_id)
                    require("lsp-inlayhints").on_attach(client, bufnr)
                end,
            })

            vim.api.nvim_create_user_command("InlayHintsToggle", function()
                require("lsp-inlayhints").toggle()
            end, { desc = "Toggle lsp inlay hints" })

            vim.keymap.set("n", "<leader>i", "<cmd>InlayHintsToggle<cr>", { desc = "Toggle inlay hints" })
        end,
    },
}