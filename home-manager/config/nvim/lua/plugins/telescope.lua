return {
  -- Fuzzy finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- Fuzzy finder algorithm
      { 'natecraddock/telescope-zf-native.nvim' }
    },
  },
}
