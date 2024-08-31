return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			-- Fuzzy finder algorithm
			{ "natecraddock/telescope-zf-native.nvim" },
			{ "jonarrien/telescope-cmdline.nvim" },
		},
		opts = {
			defaults = {
				layout_strategy = "bottom_pane",
				layout_config = {
					height = 0.5,
				},
				border = true,
				borderchars = {
					prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
					results = { " " },
					preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
				},
				sorting_strategy = "ascending",
				mappings = {
					n = {
						s = flash,
						["<c-c>"] = "close",
						["<c-g>"] = "close",
						-- ["<c-q>"] = require("trouble.providers.telescope").open_with_trouble,
					},
					i = {
						-- ["<c-j>"] = "move_selection_next",
						-- ["<c-k>"] = "move_selection_previous",
						["<c-f>"] = "to_fuzzy_refine",
						["<c-c>"] = "close",
						["<c-g>"] = "close",
						-- ["<c-o>"] = "preview_scrolling_left",
						-- ["<c-i>"] = "preview_scrolling_right",
						["<c-;>"] = "cycle_previewers_next",
						["<c-,>"] = "cycle_previewers_prev",
						-- ["<c-q>"] = require("trouble.providers.telescope").open_with_trouble,
					},
				},
			},
		},
		config = function(_, opts)
			local telescope = require("telescope")
			telescope.setup(opts)
			telescope.load_extension("zf-native")
		end,
		keys = {
			{ ":", "<cmd>Telescope cmdline<cr>", desc = "Cmdline" },
		},
	},
}
