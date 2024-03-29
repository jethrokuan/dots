;; -*- no-byte-compile: t; -*-
(package! ctrlf)
(package! dired-narrow)
(package! deadgrep)
(package! easy-kill)
(package! org-clock-convenience)
(package! company-posframe)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam" :branch "main"))
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! modus-themes)
(package! ox-hugo)
(package! ox-texinfo+
  :recipe (:host github :repo "tarsius/ox-texinfo-plus"))
(package! nov
  :recipe (:type git :repo "https://depp.brause.cc/nov.el.git"))
(package! git-link)
(package! dash)
(package! citar-org-roam)
(package! org-roam-ui)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! emacsql-sqlite-builtin)
(package! thrift)
(package! abnormal
  :recipe (:local-repo "/Users/jethro/projects/abnormal.el"))
(package! sphinx-doc)
(package! jq-mode)
(package! bazel
  :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
