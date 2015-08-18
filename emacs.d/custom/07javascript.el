(add-to-list 'auto-mode-alist '("\\.coffee.tpl$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . flymake-mode))

(require 'flymake-easy)


;; Configure Node
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
        "/usr/local/bin"
        "/usr/bin"
        ))

;; Configure jshint-mode
;; Requirement: install jshint-mode via NPM ->     npm install -g jshint-mode
(add-to-list 'load-path "/usr/local/lib/node_modules/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
          (lambda () (flymake-mode t)))
(add-hook 'js2-mode-hook
          (lambda () (flymake-mode t)))


(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)
