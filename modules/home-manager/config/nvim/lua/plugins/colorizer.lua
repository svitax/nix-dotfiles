return {
    {
        -- Highlight hex/rgb/css colors
        "NvChad/nvim-colorizer.lua",
        event = "BufReadPost",
        opts = {
            user_default_options = {
                names = false,
                mode = "virtualtext",
                virtualtext = "▋",
                always_update = true,
                rgb_fn = true,
                hsl_fn = true,
            },
        },
    },
}
