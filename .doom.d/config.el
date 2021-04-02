;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jethro Kuan"
      user-mail-address "jethrokuan95@gmail.com"
      doom-scratch-intial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "Roboto Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville")
      doom-serif-font (font-spec :family "Libre Baskerville")
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      load-prefer-newer t

      company-idle-delay nil
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

(setq org-directory "~/.org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)

(setq search-highlight t
      search-whitespace-regexp ".*?")

(use-package! orderless
  :config
  (after! ivy
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))

(use-package! ctrlf
  :hook
  (after-init . ctrlf-mode))

(setq direnv-always-show-summary nil)

(use-package! dired-narrow
    :commands (dired-narrow-fuzzy)
    :init
    (map! :map dired-mode-map
          :desc "narrow" "/" #'dired-narrow-fuzzy))

(map! "C-c h s" #'+vc/smerge-hydra/body)

(use-package! git-link
    :commands
    (git-link git-link-commit git-link-homepage)
    :custom
    (git-link-use-commit t))

(map! "s-g" #'magit-status
      "C-c g" #'magit-status
      "s-G" #'magit-blame-addition
      "C-c G" #'magit-blame-addition)

(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-barf-sexp))

(require 'org)
(require 'org-habit)

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'proselint 'org-mode))

(map! :leader
      :prefix "n"
      "c" #'org-capture)
(map! :map org-mode-map
      "M-n" #'outline-next-visible-heading
      "M-p" #'outline-previous-visible-heading)
(setq org-src-window-setup 'current-window
      org-return-follows-link t
      org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (dot . t)
                                 (R . t))
      org-confirm-babel-evaluate nil
      org-use-speed-commands t
      org-catch-invisible-edits 'show
      org-preview-latex-image-directory "/tmp/ltximg/"
      org-structure-template-alist '(("a" . "export ascii")
                                     ("c" . "center")
                                     ("C" . "comment")
                                     ("e" . "example")
                                     ("E" . "export")
                                     ("h" . "export html")
                                     ("l" . "export latex")
                                     ("q" . "quote")
                                     ("s" . "src")
                                     ("v" . "verse")
                                     ("el" . "src emacs-lisp")
                                     ("d" . "definition")
                                     ("t" . "theorem")))

(defun jethro/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
(require 'find-lisp)
(setq jethro/org-agenda-directory (file-truename "~/.org/gtd/"))
(setq org-agenda-files
      (find-lisp-find-files jethro/org-agenda-directory "\.org$"))

(setq org-capture-templates
        `(("i" "Inbox" entry (file ,(expand-file-name "inbox.org" jethro/org-agenda-directory))
           ,(concat "* TODO %?\n"
                    "/Entered on/ %u"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist '(("@errand" . ?e)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      (:newline)
                      ("CANCELLED" . ?c)))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:level . 1))))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(map! :map org-agenda-mode-map
      "i" #'org-agenda-clock-in
      "I" #'jethro/clock-in-and-advance
      "r" #'jethro/org-process-inbox
      "R" #'org-agenda-refile
      "c" #'jethro/org-inbox-capture)

(defun jethro/advance-todo ()
  (org-todo 'right)
  (remove-hook 'org-clock-in-hook #'jethro/advance-todo))

(defun jethro/clock-in-and-advance ()
  (interactive)
  (add-hook 'org-clock-in-hook 'jethro/advance-todo)
  (org-agenda-clock-in))

(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(use-package! org-agenda
  :init
  (map! "<f1>" #'jethro/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun jethro/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  (defun jethro/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

  (defun jethro/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((jethro/is-project-p)
        next-headline)
       (t
        nil)))))

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files '(,(expand-file-name "inbox.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(expand-file-name "emails.org" jethro/org-agenda-directory)))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'jethro/skip-projects)
                                              (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(expand-file-name "next.org" jethro/org-agenda-directory)))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  (setq org-roam-directory (file-truename "~/.org/braindump/org/")
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  :config
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              ;; #'org-roam-unlinked-references-insert-section
              ))
  (org-roam-setup)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+roam_key: ${ref}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t)))
  (add-to-list 'org-capture-templates `("c" "org-protocol-capture" entry (file+olp ,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory) "The List")
                                         "* TO-READ [[%:link][%:description]] %^g"
                                         :immediate-finish t))
  (add-to-list 'org-agenda-custom-commands `("r" "Reading"
                                             ((todo "WRITING"
                                                    ((org-agenda-overriding-header "Writing")
                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
                                              (todo "READING"
                                                    ((org-agenda-overriding-header "Reading")
                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
                                              (todo "TO-READ"
                                                    ((org-agenda-overriding-header "To Read")
                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory))))))))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))
  (set-company-backend! 'org-mode '(company-capf)))

(after! org-ref
  (setq org-ref-default-bibliography `,(list (concat org-directory "braindump/org/biblio.bib"))))

(use-package! org-roam-protocol
  :after org-protocol)

(after! company
  (map! "M-/" #'company-complete))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(after! (org-roam)
  (winner-mode +1)
  (map! :map winner-mode-map
        "<M-right>" #'winner-redo
        "<M-left>" #'winner-undo))

(use-package! ox-hugo
  :after org)

(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  (map! :map org-mode-map
        "s-Y" #'org-download-screenshot
        "s-y" #'org-download-yank)
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat "./images/"
                            (replace-regexp-in-string " " "_"
                                                      (downcase (file-name-base buffer-file-name)))))
           (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  :config
  (setq org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -u -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))
  (setq org-download-method '+org/org-download-method))

(use-package! citeproc-org
  :after org
  :config
  (citeproc-org-setup))

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (map! "C-x m" #'mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "flameshot gui -p %s"
        mathpix-app-id (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-id") (buffer-string))
        mathpix-app-key (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-key") (buffer-string))))

(use-package! anki-editor
  :commands (anki-editor-mode))

(use-package! gif-screencast
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package! outshine
  :commands (outshine-mode))

(after! mixed-pitch
  (dolist (f (-filter (lambda (sym)
                        (s-prefix? "company-" (symbol-name sym)))
                      (face-list)))
    (pushnew! mixed-pitch-fixed-pitch-faces f)))

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path "~/.org/braindump/org/"
        bibtex-completion-bibliography "~/.org/braindump/org/biblio.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
         (concat
          "#+title: ${title}\n"
          "#+roam_key: cite:${=key=}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":Custom_ID: ${=key=}\n"
          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
          ":AUTHOR: ${author-abbrev}\n"
          ":JOURNAL: ${journaltitle}\n"
          ":DATE: ${date}\n"
          ":YEAR: ${year}\n"
          ":DOI: ${doi}\n"
          ":URL: ${url}\n"
          ":END:\n\n"
          )))

(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(use-package! yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; (use-package! org-roam-server)

(use-package! emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)))


(defun jethro/open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(map! "C-c o o" 'jethro/open-with)

(after! org-latex
  (setq org-latex-pdf-process (list "latexmk -f -xelatex %f")))

(map!
 [backtab] #'+fold/toggle
 [C-tab] #'+fold/open-all
 [C-iso-lefttab] #'+fold/close-all)
