return {
    {
        "dgagn/diagflow.nvim",
        event = "LspAttach",
        opts = {
            toggle_event = { "InsertEnter", "InsertLeave" },
            show_sign = true,
        },
    },
}