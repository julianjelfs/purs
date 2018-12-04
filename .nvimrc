let g:neoformat_enabled_haskell = []

autocmd Filetype haskell setlocal makeprg=stack\ install\ --fast
nmap <c-b> :make<cr>
