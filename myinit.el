(server-start)

(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; show column numbers
(setq column-number-mode t)
;; get rid of startup message
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-linum-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
;; full screen by default
(set-frame-parameter nil 'fullscreen 'fullboth)
;; get rid of annoying sounds
(setq ring-bell-function 'ignore)
;; stop new frames popping up
(setq ns-pop-up-frames nil)

(use-package aggressive-indent
  :ensure t
  :init
  (global-aggressive-indent-mode 1))

(use-package try
  :ensure t)

(use-package which-key
             :ensure t
             :config
             (which-key-mode))

(defvar org-export-output-directory-prefix
 "export_"
 "prefix of directory used for org-mode export")

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
      (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
      (when (not (file-directory-p pub-dir))
       (make-directory pub-dir))))

(setq org-src-fontify-natively t)
(setq org-indent_mode nil)
(setq org-adapt-indentation nil)

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

 (use-package ivy-hydra
  :ensure t)

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

(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))

(use-package stan-mode
  :ensure t)

(use-package stan-snippets
  :ensure t)

(use-package latex-preview-pane
  :ensure t)

(use-package markdown-mode
 :ensure t
 :commands (markdown-mode gfm-mode)
 :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
      ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

(use-package projectile
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

(use-package magit
 :ensure t
 :bind (("C-x g" . magit-status)
        ("C-x M-g" . magit-dispatch-popup)))

;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
(pdf-tools-install)

(use-package ivy-bibtex
  :ensure t
  :bind (("M-i" . ivy-bibtex))
  :config
  (setq bibtex-completion-bibliography "/Users/teddy/Reading/bibliography.bib")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-library-path "/Users/teddy/Reading/pdf")
  (setq bibtex-completion-notes-path "/Users/teddy/Writing/notes/reading_notes.org"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(require 'linum)

(global-linum-mode)

(defcustom linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode doc-view-mode pdf-view-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
              (and linum-disable-starred-buffers (string-match "*" (buffer-name)))
              )
    (linum-mode 1)))

(provide 'setup-linum)

(use-package ag
  :ensure t)

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package ess
  :ensure t
  :init (require 'ess-site))

(use-package neotree
  :ensure t
  :config
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (setq neo-smart-open t)
    (setq neo-window-fixed-size nil)
    (defun neotree-project-dir ()
	"Open NeoTree using the git root."
	(interactive)
	(let ((project-dir (projectile-project-root))
	    (file-name (buffer-file-name)))
	(neotree-toggle)
	(if project-dir
	    (if (neo-global--window-exists-p)
		(progn
		    (neotree-dir project-dir)
                (neotree-find file-name)))
	    (message "Could not find git project root."))))
    (global-set-key [f8] 'neotree-project-dir)
)

(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))
