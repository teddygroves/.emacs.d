;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; package and use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(server-start)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; General purpose packages
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package aggressive-indent
  :ensure t
  :init
  (global-aggressive-indent-mode 1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :pin melpa)

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
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :after ivy
  :config
  (setq dumb-jump-selector 'ivy)
  :init
  (dumb-jump-mode))

(use-package ag
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Evil mode
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
(evil-set-initial-state 'term-mode 'emacs) ;; Use emacs mode in terminals (except ~M-x shell~)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  ;; lsp-ui gives us the blue documentation boxes and the sidebar info
  (use-package lsp-ui
    :ensure t
    :config
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  ;; make sure we have lsp-imenu everywhere we have LSP
  ;; (require 'lsp-imenu)
  ;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
    :ensure t
    :config
    (setq company-lsp-async t)
    (setq company-lsp-cache-candidates t)
    (push 'company-lsp company-backends))
  (use-package lsp-python-ms
    :demand
    :load-path "~/.emacs.d/git-repos/lsp-python-ms/"
    :ensure nil
    :hook (python-mode . lsp)
    :config
    ;; for dev build of language server
    (setq lsp-python-ms-dir
          (expand-file-name "/Users/tedgro/Code/cloned/python-language-server/output/bin/Release/"))
    ;; for executable of language server, if it's not symlinked on your PATH
    (setq lsp-python-ms-executable
          "/Users/tedgro/Code/cloned/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
  (setq lsp-enable-indentation t)
  (setq lsp-enable-completion-at-point t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-ui-flycheck-enable t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GPG
(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil)
(epa-file-enable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Filetype-specific packages
;; Stan
(use-package stan-mode
  :ensure t)
(use-package stan-snippets
  :ensure t)

;; Matlab
;; Copied from [[https://github.com/thisirs/dotemacs/blob/master/lisp/init-matlab.el][here]]. 
;; NB make sure the version is correct.
(use-package matlab
  :ensure matlab-mode
  :mode ("\\.m$" . matlab-mode)
  :config
  (setq matlab-indent-function t)
  (setq matlab-shell-command "/Applications/MATLAB_R2018b.app/bin/matlab")
  (setq matlab-shell-command-switches (list "-nodesktop")))

;; Dockerfile mode
(use-package dockerfile-mode
  :ensure t)

;; R
(use-package ess
  :ensure t
  :init (require 'ess-site))

;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

;; bibtex
(use-package ivy-bibtex
  :ensure t
  :bind (("M-i" . ivy-bibtex))
  :config
  (setq bibtex-completion-bibliography "/Users/tedgro/Dropbox/Reading/bibliography.bib")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-library-path "/Users/tedgro/Dropbox/Reading/pdf")
  (setq bibtex-completion-notes-path "/Users/tedgro/Dropbox/Writing/reading_notes/reading_notes.org"))

;; Pdf-tools
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
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (setq auto-revert-interval 0.5))
(pdf-tools-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org mode configuration

;; Use recent version of org mode with contributed packages
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(unless (package-installed-p 'org-plus-contrib)  ;; Make sure the Org package is
  (package-install 'org-plus-contrib))

;; interface
(setq org-src-fontify-natively t)
(setq org-indent_mode nil)
(setq org-adapt-indentation nil)
;; export
(use-package ox-pandoc
  :ensure t
  :init (add-to-list 'exec-path "/usr/local/bin"))
(require 'ox-publish)
(defvar org-export-output-directory-prefix  ;; Export to export_<file_type>
  "export_"
  "prefix of directory used for org-mode export")
(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

;; Clicking on ebib link opens the pdf
(org-link-set-parameters "ebib" :follow 'open-ebib)
(defun open-ebib (arg) 
  (find-file 
   (car (bibtex-completion-find-pdf arg bibtex-completion-find-additional-pdfs))))

;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)
   (shell . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Gtd
(setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files '("~/Dropbox/Writing/gtd/gtd.org"))
(setq org-capture-templates '(("i" "Inbox" entry
                               (file+headline "~/Dropbox/Writing/gtd/gtd.org" "Inbox")
                               "* TODO %i%?")))
(setq org-refile-targets '(("~/Dropbox/Writing/gtd/gtd.org" :maxlevel . 1)))

;; youtube links (Prefix is 'yt')
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(require 'ox-bibtex)
(setq org-bibtex-file "/Users/tedgro/Dropbox/Reading/bibliography.bib")
(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customisations
;; Default fill column 79
(setq-default fill-column 79)

;; Autosaves to temporary directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; images
(auto-image-file-mode 1)
(global-auto-revert-mode 1)
(add-hook 'image-mode-hook 'auto-revert-mode)

;; show column numbers
(setq column-number-mode t)

;; get rid of startup message
(setq inhibit-startup-message t)

;; get rid of annoying sounds
(setq ring-bell-function 'ignore)

;; stop new frames popping up
(setq ns-pop-up-frames nil)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Nicer window resizing  
(setq frame-resize-pixelwise t)

;; Spell check with aspell
(setq ispell-program-name "aspell")

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-linum-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; function for incrementing numbers with C-c +
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

