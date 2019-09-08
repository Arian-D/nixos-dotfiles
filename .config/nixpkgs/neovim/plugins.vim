" Change the order of supertab completions
let g:SuperTabDefaultCompletionType = "<c-n>"

" Always start deoplete
let g:deoplete#enable_at_startup = 1

" LSP integrations with LanguageClient
let g:LanguageClient_serverCommands = {
    \ 'c' :             ['cquery'],
    \ 'cpp' :           ['cquery'],
    \ 'haskell':        ['hie-wrapper'],
    \ 'rust':           ['rustup', 'run', 'nightly', 'rls'],
    \ 'javascript':     ['typescript-language-server', '--stdio'],
    \ 'javascript.jsx': ['typescript-language-server', '--stdio'],
    \ 'python':         ['pyls'],
    \ 'typescript':     ['typescript-language-server', '--stdio'],
    \ 'typescript.tsx': ['typescript-language-server', '--stdio'],
    \ 'elm':            ['elm-language-server',        '--stdio'],
    \ 'css':            ['css-languageserver',         '--stdio'],
    \ 'html':           ['html-languageserver',         '--stdio'],
\ }


" LanguageClient shortcuts (with modified leader)
function SetLSPShortcuts()
  nnoremap <leader>ld :call LanguageClient#textDocument_definition()<CR>
  nnoremap <leader>lr :call LanguageClient#textDocument_rename()<CR>
  nnoremap <leader>lf :call LanguageClient#textDocument_formatting()<CR>
  nnoremap <leader>lt :call LanguageClient#textDocument_typeDefinition()<CR>
  nnoremap <leader>lx :call LanguageClient#textDocument_references()<CR>
  nnoremap <leader>la :call LanguageClient_workspace_applyEdit()<CR>
  nnoremap <leader>lc :call LanguageClient#textDocument_completion()<CR>
  nnoremap <leader>lh :call LanguageClient#textDocument_hover()<CR>
  nnoremap <leader>ls :call LanguageClient_textDocument_documentSymbol()<CR>
  nnoremap <leader>lm :call LanguageClient_contextMenu()<CR>
endfunction()

augroup LSP
  autocmd!
  autocmd FileType cpp,c,haskell,rust,javascript,python,typescript call SetLSPShortcuts()
augroup END

autocmd FileType java setlocal omnifunc=javacomplete#Complete

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Enable those cute lil arrows
let g:airline_powerline_fonts = 1

" Syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

colorscheme gruvbox

