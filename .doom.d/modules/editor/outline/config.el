(use-package! outline
  :config
  (require 'foldout)
  (use-package! bicycle)

  (defun prot/outline-hide-all ()
    "Hide all `outline-mode' subtrees."
    (interactive)
    (outline-map-region 'outline-hide-subtree (point-min) (point-max)))

  (defun prot/outline-down-heading ()
    "Move to the next `outline-mode' subtree."
    (interactive)
    ;; Hacky, but it kinda works.
    (outline-up-heading 1 t)
    (outline-forward-same-level 1))

  (defun prot/bicycle-cycle-tab-dwim ()
    "Convenience wrapper for TAB key in `outline-mode'."
    (interactive)
    (if (outline-on-heading-p)
        (bicycle-cycle)
      (indent-for-tab-command)))

  (defvar prot/outline-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-n") 'outline-next-visible-heading)
      (define-key map (kbd "M-p") 'outline-previous-visible-heading)
      (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
      (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
      (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
      (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
      (define-key map (kbd "C-c C-a") 'outline-show-all)
      (define-key map (kbd "C-c C-q") 'prot/outline-hide-all)
      (define-key map (kbd "C-c C-u") 'outline-up-heading)
      (define-key map (kbd "C-c C-d") 'prot/outline-down-heading)
      (define-key map (kbd "C-c C-z") 'foldout-zoom-subtree)
      (define-key map (kbd "C-c C-x") 'foldout-exit-fold)
      (define-key map (kbd "<tab>") 'prot/bicycle-cycle-tab-dwim)
      (define-key map (kbd "<C-tab>") 'bicycle-cycle)
      (define-key map (kbd "<S-iso-lefttab>") 'bicycle-cycle-global)
      map)
    "Key map for `prot/outline-minor-mode'.
The idea is to make `outline-minor-mode' keys a bit easier to
work with.")

  (defvar prot/outline-minor-mode-enter-hook nil
    "Hook that runs when `prot/outline-minor-mode' is enabled.")

  (defvar prot/outline-minor-mode-exit-hook nil
    "Hook that runs when `prot/outline-minor-mode' is disabled.")

  ;; `imenu' integration with `outline-minor-mode'
  (defun prot/outline-imenu-heading ()
    "Move to the previous `outline-mode' heading.
This is because `imenu' produces its index by moving backward
from the bottom."
    (outline-previous-heading))

  (defun prot/outline-imenu-title ()
    "Return the text of the `outline-mode' heading."
    (interactive)
    ;; NOTE This may be too simplistic and error-prone, though I have
    ;; not ran into any problems.
    (buffer-substring (point-at-bol)
                      (point-at-eol)))

  (defun prot/outline-imenu-setup ()
    "`imenu' bindings for the local `outline-mode' buffer.
To be used in tandem with `prot/outline-minor-mode-enter-hook'."
    (setq-local imenu-prev-index-position-function
                'prot/outline-imenu-heading)
    (setq-local imenu-extract-index-name-function
                'prot/outline-imenu-title))

  (defun prot/outline-imenu-restore ()
    "Restore `imenu' list when `prot/outline-minor-mode' is off.
The new index should be the same as the one you would get in a
standard invocation of `imenu'.

To be used in `prot/outline-minor-mode-exit-hook'."
    (dolist (var '(imenu-prev-index-position-function
                   imenu-extract-index-name-function))
      (kill-local-variable var))
    (save-excursion
      (imenu-default-create-index-function)
      (message "Refreshed `imenu' index")))

  ;; XXX This could prove problematic when more than one major modes are
  ;; active, such as when you use C-c ' in an Org block.
  (defun prot/outline-minor-refontify ()
    "Re-enable the current buffer's major mode.
Add this to `prot/outline-minor-mode-exit-hook'."
    (let ((mode major-mode))
      (when (or (derived-mode-p 'text-mode)
                (derived-mode-p 'prog-mode))
        (funcall mode)
        (message "Re-enabled %s" mode))))

  (define-minor-mode prot/outline-minor-mode
    "Toggles `outline-minor-mode' and extras.

\\{prot/outline-minor-mode-map}"
    :init-value nil
    :lighter " =┆"
    :global nil
    :keymap prot/outline-minor-mode-map
    (if prot/outline-minor-mode
        (progn
          (when (eq major-mode 'org-mode)
            (user-error "Don't use `outline-minor-mode' with Org"))
          (outline-minor-mode 1)
          (run-hooks 'prot/outline-minor-mode-enter-hook))
      (outline-minor-mode -1)
      (run-hooks 'prot/outline-minor-mode-exit-hook)))

  :hook ((prot/outline-minor-mode-enter-hook . prot/outline-imenu-setup)
         (prot/outline-minor-mode-exit-hook . prot/outline-imenu-restore)
         (prot/outline-minor-mode-exit-hook . prot/outline-minor-refontify))
  ;; key replaces `menu-bar-open', which I never use
  :bind ("<f10>" . prot/outline-minor-mode))

(use-package! outline-minor-faces
  :hook
  (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))
