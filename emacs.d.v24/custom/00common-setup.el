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
(setq backup-inhibited t)
(setq make-backup-files nil)

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

;; keyboard scroll one line at a time
(setq scroll-step 1)

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
(load-theme 'solarized-dark t)
;(load-theme 'Deviant t)
;(load-theme 'solarized t)
;(color-theme-solarized-dark)
;(set-frame-parameter nil 'background-mode 'dark)
;(set-terminal-parameter nil 'background-mode 'dark)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (interactive)
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ========== Place Backup Files in Specific Directory ==========
;; Enable backup files.
;;(setq make-backup-files t)
(setq make-backup-files nil)

;disable backup
(setq backup-inhibited t)

;disable auto save
(setq auto-save-default nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Enable versioning with default values (keep five last versions, I think!)
;;(setq version-control t)

;; Save all backup file in this directory.
;;(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Configure Exec Path
(setq exec-path (append exec-path '("/usr/local/Cellar" "/Users/winton/.nvm/v0.11.9/bin/")))
