return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = { taplo = {} },
            setup = {
                taplo = function()
                    require("util").lsp.on_attach(function(_, buffer)
                        local function show_documentation()
                            if vim.fn.expand("%:t") == "Cargo.toml" and require("crates").popup_available() then
                                require("crates").show_popup()
                            else
                                vim.lsp.buf.hover()
                            end
                        end

                        vim.keymap.set(
                            "n",
                            "<leader>k",
                            show_documentation,
                            { buffer = buffer, noremap = true, desc = "Show docs" }
                        )
                    end)
                end,
            },
        },
    },
}
