;; -*- no-byte-compile: t; -*-
(package! ctrlf)
(package! dired-narrow)
(package! deadgrep)
(package! easy-kill)
(package! org-clock-convenience)
(package! company-posframe)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
;; (package! org-roam-bibtex
;;   :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! modus-themes)
(package! outshine)
(package! ox-hugo)
(package! ox-texinfo+
  :recipe (:host github :repo "tarsius/ox-texinfo-plus"))
(package! nov
  :recipe (:type git :repo "https://depp.brause.cc/nov.el.git"))
(package! git-link)
(package! yaml-mode)
(package! emmet-mode)
(package! dash)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-roam-bibtex)
(package! tree-sitter)
(package! tree-sitter-langs)
