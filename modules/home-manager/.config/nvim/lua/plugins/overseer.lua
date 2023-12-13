return {
    {
        "stevearc/overseer.nvim",
        dependencies = { "mfussenegger/nvim-dap" },
        opts = {
            task_list = {
                bindings = {
                    ["<C-d>"] = "ScrollOutputDown",
                    ["<C-u>"] = "ScrollOutputUp",
                    ["<leader>k"] = "TogglePreview",
                    ["p"] = nil,
                    ["<C-j>"] = nil,
                    ["<C-k>"] = nil,
                },
            },
        },
        config = function(_, opts)
            local command = vim.api.nvim_create_user_command

            require("overseer").setup(opts)
            require("dap.ext.vscode").json_decode = require("overseer.json").decode

            vim.keymap.set("n", "<leader>gt", "<cmd>OverseerOpen<cr>", { desc = "Toggle task output" })
            vim.keymap.set("n", "<leader>gl", "<cmd>OverseerRun<cr>", { desc = "Launch task" })
            -- vim.keymap.set("n", "<leader>gn", "<cmd>lua require('projector').next()<cr>", { desc = "Step to next" }) -- next task?

            -- The main :Make command from vim-dispatch can be mimicked fairly easily
            command("Make", function(params)
                local task = require("overseer").new_task({
                    cmd = vim.split(vim.o.makeprg, "%s+"),
                    args = params.fargs,
                })
                task:start()
            end, { desc = "Run your makeprg as an Overseer task", nargs = "*", bang = true })
        end,
    },
}
