;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       company
       ivy
       ;; (selectrum +prescient +orderless)

       :ui
       hydra
       doom
       doom-dashboard
       hl-todo
       modeline
       nav-flash
       ophints
       ;; (popup +all +defaults)
       vc-gutter

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
       gist
       (debugger +lsp)
       direnv
       (eval +overlay)
       (lookup +docsets +dictionary)
       (lsp +peek)
       (magit +forge)
       upload

       :lang
       data
       emacs-lisp
       (latex +latexmk)
       markdown
       nix
       (org +noter +dnd)
       (python +lsp +pyright)
       rest
       (rust +lsp)
       sh
       web

       :config
       (default +bindings +smartparens))
