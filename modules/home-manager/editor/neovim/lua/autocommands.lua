local map = vim.keymap.set
local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup
-- local command = vim.api.nvim_create_user_command

augroup("TextwidthRelativeColorcolumn", {})
autocmd("OptionSet", {
    desc = "Set colorcolumn according to textwidth.",
    group = "TextwidthRelativeColorcolumn",
    pattern = "textwidth",
    callback = function()
        if vim.v.option_new ~= 0 then
            vim.opt_local.colorcolumn = "+1"
        end
    end,
})

augroup("FixVirtualEditCursorPos", {})
autocmd("ModeChanged", {
    desc = "Keep cursor position after entering normal mode from visual mode with virtual edit enabled.",
    pattern = "[vV\x16]*:n",
    group = "FixVirtualEditCursorPos",
    callback = function()
        if vim.wo.ve:find("all") and vim.w.ve_cursor then
            vim.api.nvim_win_set_cursor(0, {
                vim.w.ve_cursor[2],
                vim.w.ve_cursor[3] + vim.w.ve_cursor[4] - 1,
            })
        end
    end,
})
autocmd("CursorMoved", {
    desc = "Record cursor position in visual mode if virtualedit is set.",
    group = "FixVirtualEditCursorPos",
    callback = function()
        if vim.wo.ve:find("all") then
            vim.w.ve_cursor = vim.fn.getcurpos()
        end
    end,
})

augroup("close_with_q", { clear = true })
autocmd("FileType", {
    desc = "Close certain filetypes with 'q'",
    group = "close_with_q",
    pattern = {
        "git",
        "dap-float",
        "CodeAction",
        "help",
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

-- augroup("toggle_rel_num", { clear = true })
-- autocmd("InsertEnter", {
--     desc = "Turn off relative line number when in insert mode",
--     callback = function()
--         if not vim.tbl_contains({ "NeogitCommitMessage", "oil" }, vim.bo[0].buftype) then
--             vim.o.relativenumber = false
--         end
--     end,
--     group = "toggle_rel_num",
-- })
-- autocmd("InsertLeave", {
--     desc = "Turn on relative line number when leaving insert mode",
--     callback = function()
--         if vim.tbl_contains({ "NeogitCommitMessage", "oil" }, vim.bo[0].buftype) then
--             vim.o.relativenumber = true
--         end
--     end,
--     group = "toggle_rel_num",
-- })

augroup("highlight_on_yank", { clear = true })
autocmd("TextYankPost", {
    desc = "Highlight selection on yank",
    callback = function()
        vim.highlight.on_yank({ higroup = "Visual" })
    end,
    group = "highlight_on_yank",
})