-- Set <space> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install `lazy.nvim` plugin manager
-- https://github.com/folke/lazy.nvim
-- See `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
local uv = vim.uv or vim.loop
-- Auto-install lazy.nvim if not present
if not uv.fs_stat(lazypath) then
	print('Installing lazy.nvim')
	vim.fn.system({
		'git',
		'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable', -- latest stable release
		lazypath
	})
	print('Done.')
end
vim.opt.rtp:prepend(lazypath)

-- Configure plugins
-- NOTE: Here is where you install your plugins
-- You can configure plugins using the `config` key.
--
-- You can also configure plugins after the setup call,
-- as they will be available in your neovim runtime.
require('lazy').setup({
	-- NOTE: The import below can automatically add your own plugins, configuration, etc, from `lua/custom/plugins/*.lua`
	-- Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going
	-- For additional information see: https://github.com/folke/lazy.nvim#structuring-your-plugins
	-- { import = 'custom.plugins' },

	{ 'rebelot/kanagawa.nvim',           opts = { background = { dark = "dragon" } } },
	{
		'neovim/nvim-lspconfig',
		dependencies = {
			-- Useful status updates for LSP
			-- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
			{ 'j-hui/fidget.nvim', opts = {} },

			-- Additional lua configuration, makes nvim stuff amazing!
			'folke/neodev.nvim',
		}
	},
	-- Autocompletion
	{
		'hrsh7th/nvim-cmp',
		dependencies = {
			-- Snippet engine and it's associated nvim-cmp source
			'L3MON4D3/LuaSnip',
			'saadparwaiz1/cmp_luasnip',

			-- Adds LSP completion capabilities
			'hrsh7th/cmp-nvim-lsp',

			-- Adds a number of user-friendly snippets
			'rafamadriz/friendly-snippets',
		}
	},
	-- Autopairing
	{
		'altermo/ultimate-autopair.nvim',
		event = { 'InsertEnter', 'CmdlineEnter' },
		branch = 'v0.6', -- recommended as each new version will have breaking changes
		opts = {
			fastwarp = { map = "<C-l>", cmap = "<C-l>", rmap = "<C-h>", rcmap = "<C-h>" },
			close = { map = "<C-0>", cmap = "<C-0>" }, -- would have liked to map this to <C-)> but i don't know how to make my terminal recognize it as a valid sequence
		},
	},
	-- Smart tab / tabout of pairs
	{ 'boltlessengineer/smart-tab.nvim', opts = {} },
	-- Navigate code with search labels and enchanced character motions
	{
		-- TODO: Telescope integration
		'folke/flash.nvim',
		event = "VeryLazy",
		opts = {
			modes = {
				search = { enabled = false },
				jump = { autojump = true },
				treesitter = { label = { after = false } },
				treesitter_search = { label = { after = false } }
			},
		},
		keys = {
			{ "s",      mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc =
			"flash" },
			{ "<CR>",   mode = { "n", "x", "o" }, function() require("flash").treesitter() end,
				                                                                                           desc =
				"flash treesitter" },
			{ "<S-CR>", mode = { "n", "x", "o" }, function() require("flash").treesitter_search() end,
				                                                                                           desc =
				"flash treesitter search" },
			{ "<C-s>",  mode = { "c" },           function() require("flash").toggle() end,
				                                                                                           desc =
				"toggle flash search" }
		}
	},
	-- Useful plugin to show you pending keybinds
	{ 'folke/which-key.nvim',  opts = {} },
	-- Adds git related signs to the gutter, as well as utilities for managing changes
	{
		'lewis6991/gitsigns.nvim',
		opts = {
			-- See `:help gitsigns.txt`
			signs = {
				add = { text = '+' },
				change = { text = '~' },
				delete = { text = '_' },
				topdelete = { text = '-' },
				changedelete = { text = '~' },
			},
		},
	},
	-- Set lualine as statusline
	{
		'nvim-lualine/lualine.nvim',
		-- See `:help lualine.txt`
		opts = {
			options = {
				icons_enabled = true,
				theme = "kanagawa",
				component_separators = "|",
				section_separators = "",
			},
		},
	},
	-- Add indentation guides even on blank lines
	{
		'lukas-reineke/indent-blankline.nvim',
		-- Enable `lukas-reineke/indent-blankline.nvim`
		-- See `:help ibl`
		main = 'ibl',
		opts = {},
	},
	-- "gc" to comment visual regions/lines
	{ 'numToStr/Comment.nvim', opts = {} },
	-- Fuzzy finder (files, lsp, etc)
	{
		'nvim-telescope/telescope.nvim',
		branch = '0.1.x',
		dependencies = {
			'nvim-lua/plenary.nvim',
			-- Fuzzy finder algorithm
			{ 'natecraddock/telescope-zf-native.nvim' }
		},
	},

	-- Highlight, edit, and navigate code
	{
		'nvim-treesitter/nvim-treesitter',
		version = false,
		dependencies = {
			'nvim-treesitter/nvim-treesitter-textobjects',
		},
		-- Configure Treesitter
		-- See `:help nvim-treesitter`
		-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
		opts = {
			highlight = { enable = true, additional_vim_regex_highlighting = { "markdown" } },
			indent = { enable = true },
			-- incremental_selection = {
			-- 	enable = true,
			-- 	keymaps = {
			-- 		init_selection = "<CR>",
			-- 		node_incremental = "<CR>",
			-- 		scope_incremental = false,
			-- 		node_decremental = "<BS>"
			-- 	},
			-- }
		},
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
		end
		-- build = ":TSUpdate",
	},
	{ 'VonHeikemen/lsp-zero.nvim', branch = 'v3.x', lazy = true, config = false },
})

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Sync clipboard between OS and Neovim.
-- Remove this option if you want your OS clipboard to remain independent
-- See `:help clipboard`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- NOTE: You should make sure your terminal supports this
vim.opt.termguicolors = true

vim.cmd.colorscheme('kanagawa')

-- [[ Configure LSP ]]
-- This function gets run when an LSP connects to a particular buffer
local function on_attach(client, buffer)
	local augroup_highlight = vim.api.nvim_create_augroup("custom-lsp-references", { clear = true })
	local autocmd_clear = vim.api.nvim_clear_autocmds

	local opts = { buffer = buffer, remap = false }

	-- Enable completion triggered by <c-x><c-o>
	vim.bo[buffer].omnifunc = 'v:lua.vim.lsp.omnifunc'

	vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
	vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
	vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
	vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
	vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
	vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
	vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
	vim.keymap.set('n', '<leader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
	vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
	vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
	vim.keymap.set({ 'n', 'v' }, '<leader>ca', vim.lsp.buf.code_action, opts)
	vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
	vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, opts)
end

-- Enable the following language servers
-- Feel free to add/remove any LSPs that you want here.
--
-- If you want to override the default filetypes that your language server will attach to you can
-- define the property 'filetypes' to the map in question
local language_servers = {
	lua_ls = {},
	nil_ls = {},
	pyright = {},
	yamlls = {},
	taplo = {},
}

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

for server, server_config in pairs(language_servers) do
	local config = { capabilities = capabilities, on_attach = on_attach }

	if server_config then
		for k, v in pairs(server_config) do
			config[k] = v
		end
	end

	require('lspconfig')[server].setup(config)
end

-- Configure nvim-cmp
-- See `:help cmp`
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

local t = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

cmp.setup {
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	completion = {
		completeopt = 'menu,menuone,noinsert'
	},
	mapping = cmp.mapping.preset.insert {
		['<C-n>'] = cmp.mapping.select_next_item(),
		['<C-p>'] = cmp.mapping.select_prev_item(),
		['<C-d>'] = cmp.mapping.scroll_docs(-4),
		['<C-u>'] = cmp.mapping.scroll_docs(4),
		['<CR>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.mapping.close()
				vim.api.nvim_feedkeys(t('<CR>'), "n", true)
			else
				fallback()
			end
		end),
		['<Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.confirm()
			elseif luasnip.expand_or_locally_jumpable() then
				luasnip.expand_or_jump()
			else
				fallback()
			end
		end, { "i", "s", "c" }),
		['<S-Tab>'] = cmp.mapping(function(fallback)
			if luasnip.locally_jumpable(-1) then
				luasnip.jump(-1)
			else
				fallback()
			end
		end, { "i", "s", "c" }),
	},
	sources = {
		{ name = 'nvim_lsp' },
		{ name = 'luasnip' },
	}
}
