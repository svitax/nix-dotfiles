-- Set <space> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Install `lazy.nvim` plugin manager
-- https://github.com/folke/lazy.nvim
-- See `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local uv = vim.uv or vim.loop
-- Auto-install lazy.nvim if not present
if not uv.fs_stat(lazypath) then
    print("Installing lazy.nvim")
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
    print("Done.")
end
vim.opt.rtp:prepend(lazypath)

-- Configure plugins
-- NOTE: Here is where you install your plugins
-- You can configure plugins using the `config` key.
--
-- You can also configure plugins after the setup call,
-- as they will be available in your neovim runtime.
require("lazy").setup({
    ui = { border = "rounded" },
    change_detection = { notify = false }, -- Don't bother me when tweaking plugins
    -- NOTE: The import below can automatically add your own plugins, configuration, etc, from `lua/custom/plugins/*.lua`
    -- Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going
    -- For additional information see: https://github.com/folke/lazy.nvim#structuring-your-plugins
    { import = "plugins" },
    { import = "plugins/lang" },
})

require("options")
require("keymaps")
require("autocommands")

-- Netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.cmd([[ let g:netrw_list_hide='.*\.un\~$']])
