;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Automatically save desktop in current directory
(desktop-save-mode 1)

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
;; (require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))
(if (file-exists-p user-specific-config) (load user-specific-config))


;; Load Color-Theme
(load-file "~/.emacs.d/color-theme.el")
(color-theme-initialize)
(color-theme-ld-dark)

;; Load Php Support
(load-file "~/.emacs.d/php-mode.el")
(require 'php-mode)

;; Load Custom Functions
(load-file "~/.emacs.d/my-functions.el")

;; Setup Auto-Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Configure Auto-Complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
; Load the default configuration
(require 'auto-complete-config)
; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; Setup Javascript
(add-to-list 'load-path "~/.emacs.d/yasnippet")
;; Load the library
(require 'yasnippet)
(yas/initialize)
;; Load the snippet files themselves
(yas/load-directory "~/.emacs.d/yasnippet/snippets/text-mode")
;; Let's have snippets in the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;; Setup RSense
(setq rsense-home "~/.emacs.d/rsense-0.3")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

;; Add RSense & Auto-Complete shortcuts
;; Complete by C-c .
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))

(load-file "~/.emacs.d/haml-mode.el")
;; Add Haml-mode Shortcuts
(add-hook 'haml-mode-hook
                  '(lambda ()
                         (setq indent-tabs-mode nil)
                         (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; Load RVM support for Emacs
;; @see https://github.com/senny/rvm.el
(load-file "~/.emacs.d/rvm.el")

;; Load Lintnode
;;(add-to-list 'load-path "~/Documents/lintnode")
;;(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
;;(setq lintnode-location "~/Documents/lintnode")
;; JSLint can be... opinionated
;;(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
;;(add-hook 'js-mode-hook
;;(lambda ()
;; (lintnode-hook)))

;; Load Flymake Cursor
;;(add-to-list 'load-path "~/.emacs.d/flymake-cursor.el")
;; Nice Flymake minibuffer messages
;;(require 'flymake-cursor)

;; Line Numbers
(line-number-mode 1)

; allows syntax highlighting to work
 (global-font-lock-mode 1)

;; Load CEDET.
;; This is required by ECB which will be loaded later.
;; See cedet/common/cedet.info for configuration details.
(load-file "~/.emacs.d/cedet/common/cedet.el")

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; actionscript mode
(load-file "~/.emacs.d/actionscript-mode.el")

;;;_ , Word documents
(load-file "~/.emacs.d/no-word.el")
;; (when (locate-library "no-word")
;;   (require 'no-word)
;;   (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word)))

;; ecb menu
(add-to-list 'load-path "~/.emacs.d/ecb")
(load-file "~/.emacs.d/ecb/ecb.el")

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(ecb-layout-name "left14")
'(ecb-layout-window-sizes (quote (("left14" (0.2564102564102564 . 0.6949152542372882) (0.2564102564102564 . 0.23728813559322035)))))
'(ecb-options-version "2.40")
'(ecb-source-path (quote ("~/Desktop/hack/rails_training" "~/Desktop/hack/rails_training/community/ruby" "~/Documents/work" "~/Documents/work/andCulture/Winestore" "~/Documents/work/andCulture/GetSatisfaction" "~/Documents/work/andCulture/HatchBck/hatchbck-web")))
'(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
'(ecb-tip-of-the-day nil)
'(ecb-tree-buffer-style (quote ascii-guides)))

;; Cucumber Support
;;(load "cucumber-mode")

;; load bundle snippets
;;(yas/load-directory "~/emacs.d/cucumber.el/snippets")

;;(add-to-list 'auto-mode-alist '("\\.feature" . feature-mode))

;; Tidy HTML
(load-file "~/.emacs.d/tidy.elc")
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

(defun my-html-mode-hook () "Customize my html-mode."
  (tidy-build-menu html-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "tidy"))

(add-hook 'html-mode-hook 'my-html-mode-hook)

;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))

;; JSHint
(add-to-list 'load-path "~/.emacs.d/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
     (lambda () (flymake-mode t)))

;; Enable fly-make by default
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Ruby Mode
(add-to-list 'load-path "~/.emacs.d/ruby-mode.el") ; must be added after any path containing old ruby-mode
(setq enh-ruby-program "~/.rvm/rubies/ruby-1.9.3-p194/bin/ruby") ; so that still works if ruby points to ruby1.8
(require 'ruby-mode)

;; Less CSS Mode
(load-file "~/.emacs.d/less-css-mode.el")

;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
