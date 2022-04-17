;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-complete-file-at-point ()
  "Complete a file path from scratch at point"
  (interactive)
  (completion-in-region (point) (point) #'read-file-name-internal))

;;;###autoload
(defun +corfu-files ()
  "Complete using files source"
  (interactive)
  (let ((completion-at-point-functions (list #'+file-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-dabbrev ()
  "Complete using dabbrev source"
  (interactive)
  (let ((completion-at-point-functions (list #'+dabbrev-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-ispell ()
  "Complete using ispell source.

See `ispell-lookup-words' for more info"
  (interactive)
  (let ((completion-at-point-functions (list #'+ispell-completion-at-point-function)))
    (completion-at-point)))

;;;###autoload
(defun +corfu-dict ()
  "Complete using dict source.

See `+dict--words' for extra words, and `+dict-file' for a wordslist source "
  (interactive)
  (let ((completion-at-point-functions (list #'+dict-completion-at-point-function)))
    (completion-at-point)))
