return {
	{ "neovim/nvim-lspconfig", opts = { servers = { lua_ls = {} } } },
	{ "stevearc/conform.nvim", opts = { formatters_by_ft = { lua = { "stylua" } } } },
}
