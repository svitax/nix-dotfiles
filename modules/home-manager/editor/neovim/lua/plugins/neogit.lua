return {
	{
		"NeogitOrg/neogit",
		cmd = "Neogit",
		opts = function()
			local icons = require("utils.icons")

			return {
				-- console_timeout = 10000,
				-- auto_show_console = true,
				disable_hint = true,
				disable_commit_confirmation = "auto",
				disable_insert_on_commit = false,
				kind = "split",
				use_per_project_settings = true,
				remember_settings = true,
				graph_style = "unicode",
				status = { recent_commit_count = 20 },
				integrations = { fzf_lua = true },
				signs = {
					section = { icons.dap.collapsed, icons.dap.expanded },
					item = { icons.dap.collapsed, icons.dap.expanded },
				},
				commit_popup = { kind = "auto" },
				mappings = { popup = { ["F"] = "PullPopup", ["p"] = false } },
				sections = { recent = { folded = true }, rebase = { folded = true } },
			}
		end,
		keys = { { "<leader>m", "<cmd>Neogit<cr>", desc = "Neogit" } },
	},
}
