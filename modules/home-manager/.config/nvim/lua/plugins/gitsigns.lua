local gitsigns_icons = require("utils.icons").git.signs
return {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    {
        "lewis6991/gitsigns.nvim",
        event = "BufReadPre",
        dependencies = "nvim-lua/plenary.nvim",
        opts = {
            -- See `:help gitsigns.txt`
            signs = {
                add = { text = gitsigns_icons.add },
                change = { text = gitsigns_icons.change },
                delete = { text = gitsigns_icons.delete, show_count = true },
                topdelete = { text = gitsigns_icons.topdelete, show_count = true },
                changedelete = { text = gitsigns_icons.changedelete, show_count = true },
                untracked = { text = gitsigns_icons.untracked },
            },
            _signs_staged_enable = true,
            _signs_staged = {
                add = { text = gitsigns_icons.add },
                change = { text = gitsigns_icons.change },
                delete = { text = gitsigns_icons.delete },
                topdelete = { text = gitsigns_icons.topdelete },
                changedelete = { text = gitsigns_icons.changedelete },
            },
            count_chars = {
                [1] = "",
                [2] = "₂",
                [3] = "₃",
                [4] = "₄",
                [5] = "₅",
                [6] = "₆",
                [7] = "₇",
                [8] = "₈",
                [9] = "₉",
                ["+"] = "₊",
            },
            attach_to_untracked = true,
            max_file_length = 12000, -- lines
            on_attach = function(buffer)
                local gs = package.loaded.gitsigns

                local function map(mode, l, r, desc)
                    vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
                end

                -- stylua: ignore start
                map("n", "ga", gs.stage_hunk, "Git stage hunk")
                map("n", "gu", gs.undo_stage_hunk, "Git undo stage hunk")
                map("n", "gU", gs.reset_hunk, "Git reset hunk")
                map("x", "ga", function () gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, "Git stage hunk")
                map("x", "gu", function () gs.undo_stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, "Git undo stage hunk")
                map("x", "gU", function () gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end, "Git reset hunk")
                map("n", "gk", gs.preview_hunk, "Git preview hunk")
                map("n", "gB", function () gs.blame_line({ full = true }) end, "Git toggle blame")
                map("n", "gL", gs.toggle_current_line_blame, "Git blame line")
                map("n", "gq", function () gs.setqflist("all", {open = false}) vim.wait(50) require("fzf-lua").quickfix() end, "Git changes to qflist")
                map("", "]h", gs.next_hunk, "Next git hunk")
                map("", "[h", gs.prev_hunk, "Previous git hunk")
                map({ "o", "x" }, "ih", ":<C-u>Gitsigns select_hunk<cr>", "Gitsigns select hunk")
                -- stylua: ignore end
            end,
        },
    },
}
