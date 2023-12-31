return {
    { "folke/todo-comments.nvim", opts = { signs = false, highlight = { after = "" } } },
    -- { "chrisgrieser/nvim-puppeteer" },
    { "calops/hmts.nvim", ft = { "nix" }, opts = {} },
    -- Highlight, edit, and navigate code
    {
        "nvim-treesitter/nvim-treesitter",
        version = false,
        event = "FileType",
        cmd = {
            "TSInstall",
            "TSInstallSync",
            "TSInstallInfo",
            "TSUninstall",
            "TSUpdate",
            "TSUpdateSync",
            "TSBufEnable",
            "TSBufToggle",
            "TSEnable",
            "TSToggle",
            "TSModuleInfo",
            "TSEditQuery",
            "TSEditQueryUserAfter",
        },
        dependencies = {
            "nvim-treesitter/nvim-treesitter-textobjects",
            "RRethy/nvim-treesitter-endwise",
            { "IndianBoy42/tree-sitter-just", opts = {} },
        },
        -- Configure Treesitter
        -- See `:help nvim-treesitter`
        -- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
        opts = {
            endwise = { enable = true },
            highlight = { enable = true, additional_vim_regex_highlighting = { "markdown" } },
            indent = { enable = true },
            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "<CR>",
                    node_incremental = "<CR>",
                    scope_incremental = false,
                    node_decremental = "<BS>",
                },
            },
        },
        config = function(_, opts)
            require("nvim-treesitter.configs").setup(opts)
        end,
        -- build = ":TSUpdate",
    },
}
