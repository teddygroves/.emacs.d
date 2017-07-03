(setq inhibit-startup-message t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-linum-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package try
  :ensure t)

(use-package which-key
             :ensure t
             :config
             (which-key-mode))

;; org-bullets
;; (use-package org-bullets
;;  :ensure t
;;  :config
;;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode nil))))
(setq org-src-fontify-natively t)
(setq org-indent_mode nil)
;; markdown export
(require 'ox-md nil t)

(use-package counsel
 :ensure t
 :bind
 (("M-y" . counsel-yank-pop)
  :map ivy-minibuffer-map
  ("M-y" . ivy-next-line)))

(use-package ivy
 :ensure t
 :diminish (ivy-mode)
 :bind (("C-x b" . ivy-switch-buffer))
 :config
 (ivy-mode 1)
 (setq ivy-use-virtual-buffers t)
 (setq ivy-display-style 'fancy))

(use-package swiper
 :ensure t
 :bind (("C-s" . swiper)
	("C-r" . swiper)
	("C-c C-r" . ivy-resume)
	("M-x" . counsel-M-x)
	("C-x C-f" . counsel-find-file))
 :config
 (progn
   (ivy-mode 1)
   (setq ivy-use-virtual-buffers t)
   (setq ivy-display-style 'fancy)
   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
   ))

;; (use-package avy
;; :ensure t
;; :bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

(use-package auto-complete
 :ensure t
 :init
 (progn
   (ac-config-default)
   (global-auto-complete-mode t)
   ))

;; zenburn
 (use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

;; wheatgrass
;; (load-theme 'wheatgrass t)

(use-package ox-reveal
:ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
:ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package stan-mode
  :ensure t)

(use-package stan-snippets
  :ensure t)

(use-package latex-preview-pane
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
(setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
(setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)

  :init
  (dumb-jump-mode)
    :ensure
)
