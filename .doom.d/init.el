;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       vertico

       :ui
       hydra
       doom
       doom-dashboard
       hl-todo
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       vc-gutter
       zen

       :editor
       fold
       (format +onsave)
       snippets

       :emacs
       dired
       electric
       vc

       :checkers
       (syntax +childframe)
       spell

       :tools
       biblio
       gist
       (debugger +lsp)
       direnv
       (eval +overlay)
       (lookup +docsets +dictionary)
       (lsp +peek)
       (magit +forge)
       ;; upload

       :lang
       data
       emacs-lisp
       (latex +latexmk)
       markdown
       nix
       (org +dnd)
       (python +lsp +pyright)
       rest
       ;; (rust +lsp)
       sh
       web
       (javascript +lsp)

       :email
       ;; (mu4e +gmail)

       :config
       (default +bindings +smartparens))
