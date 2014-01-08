(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
(setq projectile-switch-project-action 'projectile-dired)

;; Press Command-t for fuzzy find in project
(global-set-key (kbd "s-t") 'projectile-find-file)

;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

;; Press Command-escape to invalidate project cache
(global-set-key (kbd "s-<escape>") 'projectile-invalidate-cache)

;; Press Control-Shift-f to grep project
(global-set-key (kbd "C-S-f") 'projectile-grep)

;; Press Command-/ to switch projects
(global-set-key (kbd "s-/") 'projectile-switch-project)
