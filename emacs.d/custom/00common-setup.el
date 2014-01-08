(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(scroll-bar-mode -1)

;; Disable backups
(setq make-backup-files nil)
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Automatically save desktop in current directory
(desktop-save-mode 1)
(setq stack-trace-on-error t)

;; Automatically remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Disable soft-wrap
(setq truncate-partial-width-windows nil)

;; Enable left-column line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Disable word-wrapping
(setq default-truncate-lines t)
(auto-fill-mode -1)

;; Line Numbers
(line-number-mode 1)
(global-font-lock-mode 1)

;; Key Remapping
(global-set-key "\C-x\C-b" 'buffer-menu) ;; open buffer menu in current window
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'uncomment-region)
(global-set-key [f5] 'indent-region)

(global-set-key "\C-xt" 'select-frame-by-name)

;; Themes
(color-theme-initialize)
;;(color-theme-ld-dark)
(require 'color-theme-solarized)
(load-theme 'solarized-dark t)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (interactive)
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
