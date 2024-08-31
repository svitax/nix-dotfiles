return {
    {
        "Bekaboo/dropbar.nvim",
        opts = function()
            local bar = require("dropbar.bar")
            local sources = require("dropbar.sources")
            local configs = require("dropbar.configs")
            local icon_kind_opts = configs.opts.icons.kinds

            local filename = {
                get_symbols = function(buf, win, cursor)
                    local symbols = sources.path.get_symbols(buf, win, cursor)
                    local filename = symbols[#symbols]
                    if vim.bo[buf].filetype == "oil" then
                        filename.name = ""
                        filename.icon = ""
                    end
                    return { filename }
                end,
            }

            local directory = {
                get_symbols = function(buf, win, cursor)
                    return {
                        bar.dropbar_symbol_t:new({
                            icon = icon_kind_opts.symbols.Folder,
                            icon_hl = "DropBarIconKindFolder",
                            name = vim.fn.expand("%:p:h:h:t"),
                            name_hl = "DropBarKindFolder",
                        }),
                        bar.dropbar_symbol_t:new({
                            icon = icon_kind_opts.symbols.Folder,
                            icon_hl = "DropBarIconKindFolder",
                            name = vim.fn.expand("%:p:h:t"),
                            name_hl = "DropBarKindFolder",
                        }),
                    }
                end,
            }
            return {
                bar = {
                    sources = function(buf)
                        return vim.bo[buf].ft == "markdown" and { sources.markdown }
                            or {
                                require("dropbar.utils").source.fallback({
                                    sources.lsp,
                                    sources.treesitter,
                                }),
                            }
                    end,
                    -- sources = function(buf, _)
                    --     if vim.bo[buf].ft == "terminal" then
                    --         return { sources.terminal }
                    --     end
                    --     if vim.bo[buf].ft == "markdown" then
                    --         return { sources.markdown }
                    --     end
                    --     return { directory, filename }
                    -- end,
                },
                sources = {
                    path = {
                        modified = function(sym)
                            return sym:merge({
                                name = sym.name .. "[+]",
                            })
                        end,
                    },
                },
                general = {
                    update_interval = 32,
                    enable = function(buf, win)
                        return not vim.api.nvim_win_get_config(win).zindex
                            and vim.bo[buf].buftype == ""
                            and vim.api.nvim_buf_get_name(buf) ~= ""
                            and not vim.wo[win].diff
                            and vim.filetype ~= "terminal"
                            and not vim.api.nvim_buf_get_name(buf):find("Neogit")
                            and not vim.api.nvim_buf_get_name(buf):find("Trouble")
                            and not vim.api.nvim_buf_get_name(buf):find("COMMIT_EDITMSG")
                    end,
                },
            }
        end,
        config = function(_, opts)
            require("dropbar").setup(opts)
            local api = require("dropbar.api")
            vim.keymap.set("n", "<Leader>;", api.pick, { desc = "Winbar picker" })
            vim.keymap.set("n", "[C", api.goto_context_start)
            vim.keymap.set("n", "]C", api.select_next_context)
        end,
    },
}