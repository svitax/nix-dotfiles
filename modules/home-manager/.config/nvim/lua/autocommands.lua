local map = vim.keymap.set
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
-- local command = vim.api.nvim_create_user_command

augroup("close_with_q", { clear = true })
autocmd("FileType", {
    desc = "Close certain filetypes with 'q'",
    group = "close_with_q",
    pattern = {
        "git",
        "dap-float",
        "CodeAction",
        "",
    },
    callback = function(event)
        vim.bo[event.buf].buflisted = false
        map("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
    end,
})

augroup("checktime", { clear = true })
autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
    desc = "Reload file if it changes on disk",
    group = "checktime",
    command = "checktime",
})

augroup("lastloc", { clear = true })
autocmd("BufReadPost", {
    desc = "Go to last location when opening a buffer",
    group = "lastloc",
    callback = function()
        local mark = vim.api.nvim_buf_get_mark(0, '"')
        local lcount = vim.api.nvim_buf_line_count(0)
        if mark[1] > 0 and mark[1] <= lcount then
            pcall(vim.api.nvim_win_set_cursor, 0, mark)
        end
    end,
})

-- setting this in vim.opt.formatoptions or set formatoptions-=cro in options.lua doesn't work for some reason
augroup("format_options", {})
autocmd("FileType", {
    desc = "Disable auto commenting of new lines",
    group = "format_options",
    command = "set formatoptions-=cro",
})

augroup("open_pdf", { clear = true })
autocmd("BufReadPost", {
    desc = "Open PDFs using Neovim with your xdg assigned viewer",
    pattern = "*.pdf",
    group = "open_pdf",
    callback = function(event)
        local filename = event.file
        vim.fn.jobstart({ "xdg-open", filename }, { detach = true })
        vim.api.nvim_command(event.buf .. "Bwipeout")
    end,
})

augroup("clear_cmd_message", { clear = true })
autocmd("CmdlineLeave", {
    desc = "Automatically clear cmd message",
    group = "clear_cmd_message",
    callback = function()
        vim.fn.timer_start(5000, function()
            vim.cmd([[echon '']])
        end)
    end,
})

augroup("toggle_rel_num", { clear = true })
autocmd("InsertEnter", {
    desc = "Turn off relative line number when in insert mode",
    callback = function()
        if not vim.tbl_contains({ "NeogitCommitMessage" }, vim.bo[0].buftype) then
            vim.o.relativenumber = false
        end
    end,
    group = "toggle_rel_num",
})
autocmd("InsertLeave", {
    desc = "Turn on relative line number when leaving insert mode",
    callback = function()
        if vim.tbl_contains({ "NeogitCommitMessage" }, vim.bo[0].buftype) then
            vim.o.relativenumber = true
        end
    end,
    group = "toggle_rel_num",
})

augroup("highlight_on_yank", { clear = true })
autocmd("TextYankPost", {
    desc = "Highlight selection on yank",
    callback = function()
        vim.highlight.on_yank({ higroup = "Visual" })
    end,
    group = "highlight_on_yank",
})

augroup("telescope_on_enter", { clear = true })
autocmd("VimEnter", {
    desc = "Open Telescope on VimEnter if directory",
    callback = function()
        local buffer_path = vim.fn.argv(0)
        if vim.fn.isdirectory(buffer_path) == 1 or vim.bo.filetype == "oil" then
            -- local ts = require("util").telescope("files")
            -- ts()
            require("fzf-lua").files({ cwd = vim.loop.cwd() })
        end
    end,
})
