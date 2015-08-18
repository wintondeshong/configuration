(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.haml.tpl$" . haml-mode))

(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(setq scss-compile-at-save nil)
