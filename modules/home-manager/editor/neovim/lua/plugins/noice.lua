return {
	{
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			presets = {
				bottom_search = true,
				long_message_to_split = true,
				inc_rename = false,
				lsp_doc_border = true,
				command_palette = false,
			},
			cmdline = { enabled = false, view = "cmdline" },
			messages = { enabled = false },
			-- cmdline = { enabled = true, view = "cmdline" }, -- i don't like how this replaces my statusline and the hlgs
			-- messages = { enabled = true }, -- this automatically sets cmdheight to 0
			popupmenu = { backend = "cmp" },
			lsp = {
				progress = { enabled = false }, -- NOTE: fidget.nvim behaves and looks nicer
				signature = { enabled = false, auto_open = { enabled = false } },
				documentation = { view = "messages" },
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true,
				},
			},
			views = {
				-- NOTE: if you have cmdheight>0, mini will overlap with the modeline.
				-- offset the default row position for mini (-1) by your cmdheight
				mini = { position = { row = -2 } },
				notify = { merge = true, replace = true },
			},
			routes = {
				{
					-- reroute long notifications to splits
					filter = { event = "notify", min_height = 10 },
					view = "split",
				},
			},
		},
		dependencies = { "MunifTanjim/nui.nvim" },
	},
}
