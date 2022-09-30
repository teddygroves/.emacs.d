;;;; configuration of window management


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use S-<arrow-key> to move between windows (the
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modifier can be customised I think...)
 (use-package windmove
  :init
  (windmove-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; winner mode: return to previous window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; configuration with C-c <left>
(use-package winner
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; switch windows with M-o
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package bookmark+
  :straight (:type git :host github :repo "emacsmirror/bookmark-plus")
  :ensure t)



