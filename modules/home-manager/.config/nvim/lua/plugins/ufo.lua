return {
    {
        "kevinhwang91/nvim-ufo",
        dependencies = { "kevinhwang91/promise-async" },
        event = "BufReadPost",
        opts = function()
            local icons = require("utils.icons").folds
            local ellipsis = require("utils.icons").ellipsis

            local handler = function(virtText, lnum, endLnum, width, truncate)
                local newVirtText = {}
                local foldedLines = endLnum - lnum
                -- local totalLines = vim.api.nvim_buf_line_count(0)
                -- local suffix = (" " .. icons.fold .. " %d %d%%"):format(foldedLines, foldedLines / totalLines * 100)
                local suffix = (" " .. icons.suffix .. " %d"):format(foldedLines)
                local sufWidth = vim.fn.strdisplaywidth(suffix)
                local targetWidth = width - sufWidth
                local curWidth = 0
                for _, chunk in ipairs(virtText) do
                    local chunkText = chunk[1]
                    local chunkWidth = vim.fn.strdisplaywidth(chunkText)
                    if targetWidth > curWidth + chunkWidth then
                        table.insert(newVirtText, chunk)
                    else
                        chunkText = truncate(chunkText, targetWidth - curWidth)
                        local hlGroup = chunk[2]
                        table.insert(newVirtText, { chunkText, hlGroup })
                        chunkWidth = vim.fn.strdisplaywidth(chunkText)
                        -- str width returned from truncate() may less than 2nd argument, need padding
                        if curWidth + chunkWidth < targetWidth then
                            suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
                        end
                        break
                    end
                    curWidth = curWidth + chunkWidth
                end
                local rAlignAppndx = math.max(math.min(vim.opt.textwidth["_value"], width - 1) - curWidth - sufWidth, 0)
                suffix = " " .. ellipsis .. "  " .. (" "):rep(rAlignAppndx) .. suffix
                table.insert(newVirtText, { suffix, "Comment" })
                return newVirtText
            end
            return {
                open_fold_hl_timeout = 400,
                fold_virt_text_handler = handler,
                -- close_fold_kinds = { "imports", "comment" },
            }
        end,
        init = function()
            -- fold settings required for UFO
            vim.opt.foldcolumn = "1"
            vim.opt.foldenable = true
            vim.opt.foldlevel = 99
            vim.opt.foldlevelstart = 99
        end,
        config = function(_, opts)
            local ufo = require("ufo")
            ufo.setup(opts)

            vim.keymap.set("n", "<s-tab>", "za", { desc = "Fold cycle" })
            vim.keymap.set("n", "zM", ufo.closeAllFolds, { desc = "Close all folds" })
            vim.keymap.set("n", "zR", ufo.openAllFolds, { desc = "Open all folds" })
            vim.keymap.set("n", "zr", ufo.openFoldsExceptKinds, { desc = "Open all folds except kinds" })

            -- Persistent folds
            local save_fold = vim.api.nvim_create_augroup("Persistent folds", { clear = true })
            vim.api.nvim_create_autocmd("BufWinLeave", {
                pattern = "*.*",
                callback = function()
                    vim.cmd.mkview()
                end,
                group = save_fold,
            })
            vim.api.nvim_create_autocmd("BufWinEnter", {
                pattern = "*.*",
                callback = function()
                    vim.cmd.loadview({ mods = { emsg_silent = true } })
                end,
                group = save_fold,
            })
        end,
    },
}
