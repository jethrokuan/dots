;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       (vertico +orderless +icons)

       :ui
       hydra
       doom
       doom-dashboard
       hl-todo
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +defaults)
       vc-gutter
       zen

       :editor
       fold
       (format +onsave)
       snippets

       :emacs
       dired
       ibuffer
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
       lsp
       (magit +forge)
       ;; upload

       :lang
       data
       emacs-lisp
       (latex +latexmk)
       markdown
       ;; nix
       (org +dnd)
       (python +lsp +pyright)
       rest
       ;; (rust +lsp)
       sh
       web
       (javascript +lsp)
       (json +lsp)
       (go +lsp +tree-sitter)
       (yaml +lsp)

       :config
       (default +bindings +smartparens))
