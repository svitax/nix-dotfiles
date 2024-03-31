local adapters = {
    python = function()
        -- require("dap-python").setup(nil, { include_configs = false })
        require("dap-python").setup()
    end,
}
return {
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            "rcarriga/nvim-dap-ui",
            "nvim-neotest/nvim-nio",
            "mfussenegger/nvim-dap-python",
            "LiadOz/nvim-dap-repl-highlights",
        },
        config = function()
            local icons = require("utils.icons").dap

            -- Dap icon customization
            -- stylua: ignore start
            vim.fn.sign_define( "DapBreakpoint", { text = icons.breakpoint, texthl = "ErrorMsg", linehl = "", numhl = "" })
            vim.fn.sign_define( "DapBreakpointCondition", { text = icons.breakpoint_condition, texthl = "ErrorMsg", linehl = "", numhl = "" })
            vim.fn.sign_define("DapLogPoint", { text = icons.log_point, texthl = "ErrorMsg", linehl = "", numhl = "" })
            vim.fn.sign_define( "DapStopped", { text = icons.stopped, texthl = "ErrorMsg", linehl = "Substitute", numhl = "Substitute" })
            vim.fn.sign_define( "DapBreakpointRejected", { text = icons.breakpoint_rejected, texthl = "WarningMsg", linehl = "", numhl = "" })
            -- stylua: ignore end

            require("nvim-dap-repl-highlights").setup()
            -- require("nvim-dap-virtual-text").setup()

            -- Set up the ui
            require("dapui").setup({
                icons = {
                    expanded = icons.expanded,
                    collapsed = icons.collapsed,
                    current_frame = icons.stack_frame_current,
                },
                mappings = {
                    expand = { "<CR>", "<2-LeftMouse>" },
                    open = "o",
                    remove = "d",
                    edit = "c",
                    repl = "r",
                    toggle = "t",
                },
                layouts = {
                    { elements = { "breakpoints", "stacks", "scopes" }, size = 38, position = "left" },
                    { elements = { "repl", "watches" }, size = 0.24, position = "bottom" },
                },
                controls = {
                    enabled = true,
                    -- Display controls in this element
                    element = "repl",
                    icons = {
                        pause = icons.pause,
                        play = icons.play,
                        step_into = icons.step_into,
                        step_over = icons.step_over,
                        step_out = icons.step_out,
                        step_back = icons.step_back,
                        run_last = icons.run_last,
                        terminate = icons.terminate,
                    },
                },
            })

            -- stylua: ignore start
            vim.keymap.set( "n", "<leader>gr", "<cmd>lua require('dap').restart()<cr>", { desc = "Restart debugging session" }) -- restart task
            vim.keymap.set( "n", "<leader>gb", "<cmd>lua require('dap').toggle_breakpoint()<cr>", { desc = "Toggle breakpoint" })
            vim.keymap.set( "n", "<leader>gc", "<cmd>lua require('dap').continue()<cr>", { desc = "Continue program execution" }) -- continue task
            vim.keymap.set("n", "<leader>gh", "<cmd>lua require('dap').pause()<cr>", { desc = "Pause program execution" })
            vim.keymap.set("n", "<leader>gi", "<cmd>lua require('dap').step_into()<cr>", { desc = "Step in" })
            vim.keymap.set("n", "<leader>go", "<cmd>lua require('dap').step_out()<cr>", { desc = "Step out" })
            vim.keymap.set("n", "<leader>gw", "<cmd>lua require('dap').step_over()<cr>", { desc = "Step over" })
            vim.keymap.set("n", "<leader>gv", "<cmd>FzfLua dap_variables<cr>", { desc = "List variables" })
            vim.keymap.set( "n", "<leader>gx", "<cmd>lua require('dap').terminate()<cr>", { desc = "Kill debug session" }
            ) -- kill task
            -- vim.keymap.set("n", "<leader>g<C-c>", "dap_edit_condition", { desc = "Edit breakpoint condition on current line" }) -- set_breakpoint condition
            -- vim.keymap.set("n", "<leader>g<C-l>", "dap_edit_log", { desc = "Edit breakpoint log message on current line" }) -- set breakpoint log message
            -- vim.keymap.set("n", "<leader>gs", "", { desc = "Switch" })
            -- vim.keymap.set("n", "<leader>gst", "dap_switch_thread", { desc = "Switch current thread" })
            -- vim.keymap.set("n", "<leader>gsf", "dap_switch_stack_frame", { desc = "Switch stack frame" })
            -- vim.keymap.set("n", "<leader>ge", "dap_enable_exceptions", { desc = "Enable exception breakpoints" })
            -- vim.keymap.set("n", "<leader>gE", "dap_disable_exceptions", { desc = "Disable exception breakpoints" })
            -- dap-ui
            vim.keymap.set("n", "<leader>ge", "<cmd>lua require('dapui').eval()<cr>", { desc = "Evaluate" })
            -- stylua: ignore end

            -- Open ui on start
            require("dap").listeners.after.event_initialized["dapui_config"] = function()
                require("dapui").open()
            end
            require("dap").listeners.before.event_terminated["dapui_config"] = function()
                require("dapui").close()
            end
            require("dap").listeners.before.event_exited["dapui_config"] = function()
                require("dapui").close()
            end

            -- Initialize all debug adapters
            for adapter, config in pairs(adapters) do
                if type(config) == "function" then
                    config()
                else
                    require("dap").adapters[adapter] = config
                end
            end
        end,
    },
}
