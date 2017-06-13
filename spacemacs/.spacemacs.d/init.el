;; Custom configuration loaded after .emacs.d 


;; ----------------------------------------------
;; Backups (Disable)
;; ----------------------------------------------

(setq auto-save-default nil)
(setq backup-inhibited t)
(setq make-backup-files nil)


;; ----------------------------------------------
;; General
;; ----------------------------------------------

;; Automatically remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace) 
(column-number-mode t)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Disable soft-wrap
(setq truncate-partial-width-windows nil)

;; Line Numbers
(line-number-mode 1)
(global-linum-mode t)
(global-font-lock-mode 1)
(setq linum-format "%d ") ;; add right padding


;; ----------------------------------------------
;; CSharp-Mode
;; ----------------------------------------------

(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")
