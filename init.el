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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make sure environment variables are correct
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; (use-package solarized-theme)
;; (use-package gruvbox-theme)
;; (use-package dracula-theme)
;; (use-package zenburn-theme)
(use-package faff-theme)
;; (use-package darktooth-theme)
;; (use-package commentary-theme)

(load-theme 'faff)
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
  :init (global-company-mode)
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
  (yas-global-mode 1)
  :bind (("M-s M-s" . yas-insert-snippet)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-company-use-tng nil)
  :init (evil-collection-init))


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
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-mode"
  :ensure t
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/company-stan/"
  :ensure t
  :hook (stan-mode . company-stan-backend)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/eldoc-stan/"
  :ensure t
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/flycheck-stan/"
  :ensure t
  :hook (stan-mode . flycheck-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; stan-snippets.el
(use-package stan-snippets
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-snippets/"
  :ensure t
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; ac-stan.el
(use-package ac-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "path-to-your-repo/stan-mode/ac-stan/"
  ;; Delete the line below if using.
  :disabled t
  :hook (stan-mode . stan-ac-mode-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

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

;; csv mode
(use-package csv-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

;; R
(use-package ess
  :ensure t
  :init (require 'ess-site))

;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(use-package pyvenv
  :ensure t)

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

;; images
(setq org-image-actual-width nil)

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
;; Don't report auto-reverts
(setq auto-revert-verbose nil)

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
(fset 'yes-or-no-p 'y-or-n-p)

;; function for incrementing numbers with C-c +
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote ("," "	" ";")))
 '(custom-safe-themes
   (quote
    ("33af2d5cb040182b798c3a4ee6d16210e700a2fabaa409231e1c4a003cafd1c2" "6d9cb4d7b94002b476bfd18025c9ed62283a572b660e8fb988ea100aa6b8eeab" "8dcdc47af0290303002023237a77b5b412692d1b8658da819ab18caccfb811b5" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(package-selected-packages
   (quote
    (flycheck-stan c-eldoc eldoc-stan company-stan commentary-theme faff-theme color-theme-sanityinc-solarized color-theme-sanityinc-solarized-light evil-collection ox-tufte ox-ipynb csv-mode pipenv color-theme-sanityinc-tomorrow zenburn-theme darktooth-theme dracula-theme gruvbox-theme which-key use-package try stan-snippets solarized-theme scala-mode pyenv-mode pdf-tools ox-pandoc ox-hugo org-plus-contrib org-bullets ob-ipython neotree matlab-mode magit lsp-ui lsp-python latex-preview-pane key-chord ivy-hydra ivy-bibtex htmlize helm-bibtex flycheck exec-path-from-shell evil ess elpy ein dumb-jump dockerfile-mode counsel-projectile company-lsp auctex aggressive-indent ag ace-window)))
 '(pdf-tools-handle-upgrades nil))
;; (custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(org-level-1 ((t (:inherit default :foreground "#83a598" :height 1))))
;; '(org-level-2 ((t (:inherit default :foreground "#fabd2f" :height 1))))
;; '(org-level-4 ((t (:inherit default :foreground "#fb4933" :weight normal :height 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
