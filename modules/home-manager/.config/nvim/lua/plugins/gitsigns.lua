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
            on_attach = function(buffer)
                local gs = package.loaded.gitsigns

                local function map(mode, l, r, desc)
                    vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
                end

        -- stylua: ignore start
        map("n", "mz", gs.stage_hunk, "Stage hunk")
        map("n", "mx", gs.undo_stage_hunk, "Undo stage hunk")
        map("v", "mz", function () gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, "Stage hunk")
        map("v", "mx", function () gs.undo_stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end, "Undo stage hunk")
        map("", "]h", gs.next_hunk, "Next hunk")
        map("", "[h", gs.prev_hunk, "Previous hunk")
        map({ "o", "x" }, "ih", ":<C-u>Gitsigns select_hunk<cr>", "Gitsigns select hunk")
                -- stylua: ignore end
            end,
        },
    },
}
