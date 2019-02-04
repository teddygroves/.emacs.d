(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(server-start)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(elpy-modules
   (quote
    (elpy-module-eldoc elpy-module-pyvenv elpy-module-django elpy-module-sane-defaults)))
 '(elpy-shell-echo-input nil)
 '(markdown-command "/usr/local/bin/pandoc" t)
 '(org-agenda-files (quote ("~/Writing/notes/org/away_day_planning.org")))
 '(package-selected-packages
   (quote
    (emacs-matlab matlab matlab-mode matlab-model auctex solarized-theme ob-ipython ensime ace-window exec-path-from-shell elpy ein neotree ess scala-mode ag flycheck aggressive-indent ivy-bibtex pdf-tools dumb-jump counsel-projectile projectile zenburn-theme which-key use-package try stan-snippets ox-reveal org-bullets markdown-mode magit latex-preview-pane ivy-hydra htmlize evil counsel color-theme avy auto-complete)))
 '(pdf-tools-handle-upgrades nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
