return {
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		opts = {
			filter = function(mapping)
				return mapping.desc and mapping.desc ~= ""
			end,
			plugins = {
				marks = false,
				registers = false,
				spelling = {
					enabled = false,
				},
				-- TODO: disable all g and d from other plugins or add which_key_ignore/ignore_missing
				presets = {
					motions = true,
					operators = true,
				},
			},
			preset = "helix",
		},
	},
}
