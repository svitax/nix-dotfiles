return {
	-- Navigate code with search labels and enchanced character motions
	{
		-- TODO: Telescope integration
		-- TODO: flash hg don't go italic
		"folke/flash.nvim",
		opts = {
			jump = {
				-- automatically jump when there is only one match
				autojump = false,
				-- clear highlight after jump
				nohlsearch = false,
			},
			highlight = {
				-- show a backdrop with hl FlashBackdrop
				backdrop = false,
			},
			prompt = { enabled = false },
			modes = {
				search = { enabled = false },
				treesitter = { label = { after = false } },
				treesitter_search = { label = { after = false } },
			},
		},
		keys = {
			{ "f" },
			{ "F" },
			{ "t" },
			{ "t" },
			{
				"s",
				mode = { "n", "x", "o" },
				function()
					require("flash").jump()
				end,
				desc = "flash",
			},
		},
	},
}

