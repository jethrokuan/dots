;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
(use-package! exwm
  :config
  (setq exwm-workspace-number 6))

(defun jethro/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer (format "%s - %s" exwm-class-name exwm-title)))
(add-hook 'exwm-update-title-hook 'jethro/exwm-rename-buffer-to-title)
(add-hook 'exwm-update-class-hook
          (defun my-exwm-update-class-hook ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name)
                        (string= "Firefox" exwm-class-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (defun my-exwm-update-title-hook ()
            (cond ((or (not exwm-instance-name)
                       (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                       (string= "gimp" exwm-instance-name)
                       (string= "Firefox" exwm-class-name))
                   (exwm-workspace-rename-buffer exwm-title)))))

(setq exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t)

(push ?\s-  exwm-input-prefix-keys)
(push ?\M-  exwm-input-prefix-keys)

(display-time-mode 1)

(defun jethro/launch (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun jethro/screen-to-clipboard ()
  (interactive)
  (shell-command
   (concat "bash -c 'FILENAME=$(date +'%Y-%m-%d-%H:%M:%S').png && maim -u -s $FILENAME"
           " && xclip $FILENAME -selection clipboard "
           "-t image/png &> /dev/null && rm $FILENAME'"))
  (message "Added to clipboard."))

(defun jethro/switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(exwm-input-set-key (kbd "s-SPC") #'jethro/launch)
(exwm-input-set-key (kbd "s-p") #'password-store-copy)
(exwm-input-set-key (kbd "C-x t") #'vterm)
(exwm-input-set-key (kbd "s-t a") #'jethro/switch-to-agenda)
(exwm-input-set-key (kbd "s-t m") #'notmuch)
(exwm-input-set-key (kbd "s-c") #'jethro/org-inbox-capture)
(exwm-input-set-key (kbd "s-f") #'counsel-find-file)
(exwm-input-set-key (kbd "s-F") #'counsel-locate)
(exwm-input-set-key (kbd "s-<tab>") #'jethro/switch-to-last-buffer)
(exwm-input-set-key (kbd "<print>") #'jethro/screen-to-clipboard)


(mapcar (lambda (i)
          (exwm-input-set-key (kbd (format "s-%d" i))
                              `(lambda ()
                                 (interactive)
                                 (exwm-workspace-switch-create ,i))))
        (number-sequence 0 9))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "URxvt"))
              (exwm-input-set-local-simulation-keys '(([?\C-c ?\C-c] . ?\C-c))))))

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; copy/paste.
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(when (executable-find "brightnessctl")
  (defun jethro/return-brightness-percentage ()
    (interactive)
    (string-to-number (shell-command-to-string "brightnessctl get")))
  (defun jethro/brightness-up ()
    (interactive)
    (shell-command "brightnessctl set 100+")
    (message "Screen Brightness: %s" (jethro/return-brightness-percentage))
    (kill-buffer "*Shell Command Output*"))
  (defun jethro/brightness-down ()
    (interactive)
    (shell-command "brightnessctl set 100-")
    (message "Screen Brightness: %s" (jethro/return-brightness-percentage))
    (kill-buffer "*Shell Command Output*"))
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'jethro/brightness-down)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'jethro/brightness-up))

(define-ibuffer-column exwm-class (:name "Class")
  (if (bound-and-true-p exwm-class-name)
      exwm-class-name
    ""))
(define-ibuffer-column exwm-instance (:name "Instance")
  (if (bound-and-true-p exwm-instance-name)
      exwm-instance-name
    ""))
(define-ibuffer-column exwm-urgent (:name "U")
  (if (bound-and-true-p exwm--hints-urgency)
      "U"
    " "))

(defun jethro/exwm-ibuffer (&optional other-window)
  (interactive "P")
  (let ((name (buffer-name)))
    (ibuffer other-window
             "*exwm-ibuffer*"
             '((mode . exwm-mode))
             nil nil nil
             '((mark exwm-urgent
                     " "
                     (name 64 64 :left :elide)
                     " "
                     (exwm-class 20 -1 :left)
                     " "
                     (exwm-instance 10 -1 :left))))
    (ignore-errors (ibuffer-jump-to-buffer name))))

(exwm-input-set-key (kbd "s-b") #'jethro/exwm-ibuffer)

(exwm-input-set-key (kbd "<s-up>") 'windmove-up)
(exwm-input-set-key (kbd "<s-down>") 'windmove-down)
(exwm-input-set-key (kbd "<s-right>") 'windmove-right)
(exwm-input-set-key (kbd "<s-left>") 'windmove-left)

(define-key exwm-mode-map (kbd "C-x 4 0")
  (lambda ()
    (interactive)
    (kill-buffer)
    (delete-window)))

(add-hook 'exwm-manage-finish-hook
          (defun my-exwm-urxvt-simulation-keys ()
            (when exwm-class-name
              (cond
               ((string= exwm-class-name "Firefox")
                (exwm-input-set-local-simulation-keys
                 `(,@exwm-input-simulation-keys
                   ([?\C-w] . [?\C-w]))))))))

(when (file-exists-p "/home/jethro/.screenlayout/desktop.sh")
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(1 "USB-C-0" 2 "HDMI-0"))
  (call-process "bash" nil 0 nil "-c" "/home/jethro/.screenlayout/desktop.sh")
  (exwm-randr-enable))

(exwm-enable)
