local opt = vim.opt
local o = vim.o
local g = vim.g
local wo = vim.wo

-- [[ Setting options ]]
-- See `:help vim.o`

opt.path = vim.o.path .. "**" -- Search down into subfolders

opt.autowrite = true -- Enable auto write
opt.backup = false
opt.breakindent = true -- Enable break indent
opt.cmdheight = 1
opt.conceallevel = 1 -- Hide * markup for bold and italic
opt.confirm = true -- Confirm to save changes before exiting modified buffer
opt.completeopt = "menuone,noselect"
opt.colorcolumn = "120"
opt.cursorline = true -- Enable highlighting of the current line
opt.expandtab = true -- Use spaces instead of tabs
-- opt.foldmethod = "expr"
-- opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.fillchars:append("diff:╱")
opt.fillchars:append("fold: ")
opt.fillchars:append("foldopen:")
opt.fillchars:append("foldclose:")
opt.fillchars:append("foldsep: ")
opt.formatoptions = "jcroqlnt" -- tcqj
opt.grepformat = "%f:%l%c:%m"
opt.grepprg = "rg --vimgrep"
opt.hlsearch = false -- Set highlight on search
opt.ignorecase = true -- Case insensitive searching UNLESS \C or capital in search
opt.inccommand = "nosplit" -- Preview incremental substitute
-- vim.opt.laststatus = 3 -- Global status line
opt.list = true -- Show some invisible characters (tabs...)
opt.listchars:append("lead:⋅")
opt.listchars:append("leadmultispace:⋅")
opt.listchars:append("trail:⋅")
opt.listchars:append("eol:󰌑")
opt.listchars:append("tab:» ")
opt.mouse = "a" -- Enable mouse mode
opt.number = true -- Print line number
opt.pumheight = 10 -- Maximum number of entries in a popup
opt.relativenumber = true -- Relative line numbers
opt.scrolloff = 4 -- Lines of context
opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp", "folds" }
opt.shiftround = true -- Round indent
opt.shiftwidth = 4 -- Size of an indent
opt.shortmess = "filnxtToOFSc"
--- opt.shortmess = o.shortmess .. "c"
opt.showmode = false -- Don't show mode since we have it on the statusline
opt.sidescrolloff = 8 -- Columns of context
opt.signcolumn = "yes" -- Always show the signcolumn, otherwise it would shift the text each time
opt.smartcase = true
opt.swapfile = false
opt.termguicolors = true -- NOTE: You should make sure your terminal supports this
opt.timeoutlen = 300
opt.tabstop = 4 -- Identation of 4 spaces
opt.undofile = true -- Save undo history
opt.updatetime = 250 -- Decrease update time
wo.signcolumn = "yes" -- Keep signcolumn on by default
-- Sync clipboard between OS and Neovim.
-- Remove this option if you want your OS clipboard to remain independent
-- See `:help clipboard`
o.clipboard = "unnamedplus"
o.undodir = os.getenv("HOME") .. "/.local/state/nvim/undo"

g.editorconfig = true
