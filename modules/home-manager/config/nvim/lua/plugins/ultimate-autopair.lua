return {
    -- Autopairing
    {
        "altermo/ultimate-autopair.nvim",
        event = { "InsertEnter", "CmdlineEnter" },
        branch = "v0.6", -- recommended as each new version will have breaking changes
        opts = {
            fastwarp = { map = "<C-l>", cmap = "<C-l>", rmap = "<C-h>", rcmap = "<C-h>" },
            close = { map = "<C-0>", cmap = "<C-0>" }, -- would have liked to map this to <C-)> but i don't know how to make my terminal recognize it as a valid sequence
        },
    },
}
