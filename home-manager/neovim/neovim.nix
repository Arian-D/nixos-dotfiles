{ pkgs, lib, ... }:

with lib;
with builtins;

{
  programs.neovim = {
    enable = true;
    vimAlias = true;
    configure = {
      customRC = concatStringsSep "\n" (
        map readFile (
          map (path: (getEnv "HOME") + "/.config/nixpkgs/neovim/" + path)
            [
              "defaults.vim"
              "keybindings.vim"
              "plugins.vim"
            ]
        )
      );

      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          tagbar
          vim-polyglot
          vim-airline
          vim-devicons
          vim-colorschemes
          supertab
          auto-pairs
          vim-surround
          nerdtree
          vimtex
          vim-addon-nix
          Hoogle
          haskellConcealPlus
          vim-cute-python
          elm-vim
          rust-vim
          deoplete-nvim
          LanguageClient-neovim
          ultisnips
          vim-snippets
        ];
      };
    };
  };
}
