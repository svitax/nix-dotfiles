vim.cmd.colorscheme("gruvfox")
-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!
vim.opt.autowrite = true -- Enable auto write
vim.opt.backup = false
vim.opt.breakindent = true -- Enable break indent
vim.opt.cmdheight = 1
vim.opt.conceallevel = 1 -- Hide * markup for bold and italic
vim.opt.confirm = true -- Confirm to save changes before exiting modified buffer
vim.opt.completeopt = "menuone,noselect"
vim.opt.colorcolumn = "120"
vim.opt.cursorline = true -- Enable highlighting of the current line
vim.opt.expandtab = true -- Use spaces instead of tabs
-- vim.opt.foldmethod = "expr"
-- vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.fillchars:append("diff:╱")
vim.opt.fillchars:append("fold: ")
vim.opt.fillchars:append("foldopen:")
vim.opt.fillchars:append("foldclose:")
vim.opt.fillchars:append("foldsep: ")
vim.opt.formatoptions = "jcroqlnt" -- tcqj
vim.opt.grepformat = "%f:%l%c:%m"
vim.opt.grepprg = "rg --vimgrep"
vim.opt.hlsearch = false -- Set highlight on search
vim.opt.ignorecase = true -- Case insensitive searching UNLESS \C or capital in search
vim.opt.inccommand = "nosplit" -- Preview incremental substitute
vim.opt.laststatus = 3 -- Global status line
vim.opt.list = true -- Show some invisible characters (tabs...)
vim.opt.listchars:append("lead:⋅")
vim.opt.listchars:append("leadmultispace:⋅")
vim.opt.listchars:append("trail:⋅")
vim.opt.listchars:append("eol:󰌑")
vim.opt.listchars:append("tab:» ")
vim.opt.mouse = "a" -- Enable mouse mode
vim.opt.number = true -- Print line number
vim.opt.pumheight = 10 -- Maximum number of entries in a popup
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.scrolloff = 4 -- Lines of context
vim.opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp", "folds" }
vim.opt.shiftround = true -- Round indent
vim.opt.shiftwidth = 4 -- Size of an indent
vim.opt.shortmess = "filnxtToOFSc"
--- vim.opt.shortmess = o.shortmess .. "c"
vim.opt.showmode = false -- Don't show mode since we have it on the statusline
vim.opt.sidescrolloff = 8 -- Columns of context
vim.opt.signcolumn = "yes" -- Always show the signcolumn, otherwise it would shift the text each time
vim.opt.smartcase = true
vim.wo.signcolumn = "yes" -- Keep signcolumn on by default
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.swapfile = false
vim.opt.termguicolors = true -- NOTE: You should make sure your terminal supports this
vim.opt.timeoutlen = 300
vim.opt.tabstop = 4 -- Identation of 4 spaces
vim.opt.undodir = os.getenv("HOME") .. "/.local/state/nvim/undo"
vim.opt.undofile = true -- Save undo history
vim.opt.updatetime = 250 -- Decrease update time
-- Sync clipboard between OS and Neovim.
-- Remove this option if you want your OS clipboard to remain independent
-- See `:help clipboard`
vim.o.clipboard = "unnamedplus"
