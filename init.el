;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; package and use-package
(defvar in-termux-p
  (and (equal (system-name) "localhost")
       (not (equal user-login-name "mylaptopusername")))
    "t if we are in termux emacs.")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
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
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; choose a theme
  ;; (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; (use-package modus-vivendi-theme :ensure)
;; (use-package modus-operandi-theme
;;   :ensure t
;;   :init
;;   ;; NOTE: Everything is disabled by default.
;;   (setq modus-operandi-theme-slanted-constructs t
;;         modus-operandi-theme-bold-constructs t
;;         ;; modus-operandi-theme-fringes 'subtle ; {nil,'subtle,'intense}
;;         ;; modus-operandi-theme-3d-modeline t
;;         modus-operandi-theme-faint-syntax t
;;         ;; modus-operandi-theme-intense-hl-line t
;;         modus-operandi-theme-intense-paren-match t
;;         modus-operandi-theme-prompts 'subtle ; {nil,'subtle,'intense}
;;         modus-operandi-theme-completions 'opinionated ; {nil,'moderate,'opinionated}
;;         modus-operandi-theme-diffs nil ; {nil,'desaturated,'fg-only}
;;         modus-operandi-theme-org-blocks 'rainbow ; {nil,'greyscale,'rainbow}
;;         ;; modus-operandi-theme-variable-pitch-headings t
;;         modus-operandi-theme-rainbow-headings t
;;         ;; modus-operandi-theme-scale-headings t
;;         )
;;   :config
;;   (load-theme 'modus-operandi t))
;; (use-package minimal-theme
;;   :ensure t
;;   :config (load-theme 'minimal-light t))
  
(use-package tron-legacy-theme
  :ensure t
  :config
  (setq tron-legacy-theme-vivid-cursor t)
  (setq tron-legacy-theme-dark-fg-bright-comments t)
  (load-theme 'tron-legacy t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; General purpose packages

(add-hook 'dired-mode-hook
          (lambda () (dired-hide-details-mode)))

(use-package dired-x ;; so that C-x C-j always does dired-jump
  :demand t)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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
  (setq projectile-switch-project-action #'projectile-dired)
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
  (setq dumb-jump-selector 'ivy))

(use-package ag
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :bind (("M-s M-s" . yas-insert-snippet)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elfeed
;; (use-package elfeed
;;   :ensure t
;;   :defer t
;;   :commands (elfeed)
;;   :config
;;   (global-set-key (kbd "C-x w") 'elfeed)
;;   (setq-default elfeed-search-filter "@3-months-ago +unread +essay "))

;; (setq elfeed-feeds
;;       '("https://herbsutter.com/rss"))
;; (use-package elfeed-org
;;   :ensure t
;;   :config
;;   (elfeed-org)
;;   (setq rmh-elfeed-org-files (list "/Users/tedgro/.emacs.d/elfeed.org")))

;; (setq elfeed-show-mode-hook
;;       (lambda ()
;; 	(set-face-attribute
;;          'variable-pitch (selected-frame)
;;          :font (font-spec :family "Menlo" :size 12))
;; 	(setq fill-column 80)
;; 	(setq elfeed-show-entry-switch #'my-show-elfeed)))

;; (defun my-show-elfeed (buffer)
;;   (with-current-buffer buffer
;;     (setq buffer-read-only nil)
;;     (goto-char (point-min))
;;     (re-search-forward "\n\n")
;;     (fill-individual-paragraphs (point) (point-max))
;;     (setq buffer-read-only t))
;;   (switch-to-buffer buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'rst-mode 'emacs))

(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-company-use-tng nil)
  :init (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mode for reading epubs
(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-variable-pitch nil))

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

;; vterm
(use-package vterm
  :ensure t)

;; Stan
(use-package stan-mode
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-mode"
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
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy 1))

;;; eldoc-stan.el
(use-package eldoc-stan
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/eldoc-stan/"
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-snippets/"
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
  :init (require 'ess-site)
  :config
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG"))

(use-package poly-R  ;; for editing and exporting Rmd files
  :ensure t)

;; Python
(setq python-shell-interpreter "jupyter-console"
      python-shell-interpreter-args "--simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(setq python-shell-completion-native-enable nil)
(setq python-indent-def-block-scale 1) ;; function arguments have normal indentation

(use-package pyvenv
  :ensure t)
(use-package jupyter
  :unless in-termux-p
  :ensure t)

;; (use-package eval-in-repl
;;   :ensure t
;;   :config

;;   (require 'eval-in-repl-python)
;;   (setq eir-use-python-shell-send-string t)
;;   (add-hook 'python-mode-hook
;;             '(lambda ()
;;                (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))

(use-package elpy
  :ensure t
  :init (elpy-enable)
  :config (setq elpy-shell-echo-output nil))

;;; Python Polymode (Markdown + Python)
;;;
;;; https://stackoverflow.com/questions/52489905/emacs-polymode-for-markdown-and-python
;;; https://emacs.stackexchange.com/questions/20437/polymode-with-python-and-latex-mode/
;;;
;;; Further discussion and working solution at...
;;;
;;; https://github.com/polymode/polymode/issues/180
 
;; define pweave polymodes
(use-package poly-noweb
  :ensure t)
(use-package poly-markdown
  :ensure t)
 
;; Python/Markdown
(defcustom pm-inner/noweb-python
  (clone pm-inner/noweb
         :name "noweb-python"
         :mode 'python-mode)
  "Noweb for Python"
  :group 'poly-innermodes
  :type 'object)
 
(define-polymode poly-pweave-mode poly-markdown-mode
  :innermodes '(pm-inner/noweb-python :inherit))
 
(add-to-list 'auto-mode-alist '("\\.pymd" . poly-pweave-mode))

;; bibtex
(use-package ivy-bibtex
  :ensure t
  :bind (("M-i" . ivy-bibtex))
  :config
  (defun bibtex-completion-format-citation-org-cite (keys)
    "Format ebib references for keys in KEYS."
    (s-join ", "
            (--map (format "cite:%s" it) keys)
            ))

  (defun bibtex-completion-insert-org-file-link (keys)
    "Insert an org mode format link to the pdf"
    (insert (s-join ", "
                    (cl-loop
                     for key in keys
                     for entry = (bibtex-completion-get-entry key)
                     for title = (bibtex-completion-apa-get-value "title" entry)
                     for pdf = (car (bibtex-completion-find-pdf key))
                     collect (org-link-make-string pdf title)))))
  (ivy-bibtex-ivify-action ;; makes this function available to ivy-bibtex
   bibtex-completion-insert-org-file-link ivy-bibtex-insert-org-file-link)
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath)))
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-cite)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-bibliography "/Users/tedgro/Dropbox/Reading/bibliography.bib")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-library-path "/Users/tedgro/Dropbox/Reading/pdf")
  (setq bibtex-completion-notes-path "/Users/tedgro/Writing/reading_notes/reading_notes.org")
  (ivy-set-actions
   'ivy-bibtex
   '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
     ("c" ivy-bibtex-insert-citation "Insert citation")
     ("r" ivy-bibtex-insert-reference "Insert reference")
     ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
     ("l" ivy-bibtex-insert-org-file-link "Org-format link to pdf file")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org mode configuration

;; Use recent version of org mode with contributed packages
;; (unless (package-installed-p 'org-plus-contrib)  ;; Make sure the Org package is
;;(package-install 'org-plus-contrib))

;; interface
(setq org-src-fontify-natively t)
(setq org-indent_mode nil)
(setq org-adapt-indentation nil)

;; export
(use-package ox-pandoc
  :unless in-termux-p
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

;; spellchecking
(add-hook 'org-mode-hook 'flyspell-mode)

;; images
(setq org-image-actual-width nil)

;; Clicking on ebib link opens the pdf
(org-link-set-parameters "ebib" :follow 'open-ebib)
(defun open-ebib (arg) 
  (find-file 
   (car (bibtex-completion-find-pdf arg bibtex-completion-find-additional-pdfs))))

;; Org babel
(unless in-termux-p (org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (jupyter . t))))

(setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                     (:session . "py")
                                                     (:kernel . "python3")))

(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Org agenda
(setq org-agenda-files '("~/org/tasks.org"))
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-custom-commands
      '(("c" "My custom agenda view"
         ((agenda)
          (tags-todo "Paper")
          (tags-todo "Admin")
          (tags-todo "Writing")
          (tags-todo "Code")
          (tags-todo "-Paper+-Admin+-Writing+-Code")))))

;; Gtd
(setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Task" entry (file "~/org/tasks.org") "* TODO %i%?")
        ("p" "Paper" entry (file "~/org/papers.org") "* %i%?")
        ("b" "Biochem note" entry (file+headline "~/org/biochem.org" "Notes") "* %i%?")
        ("r" "Recipe" entry (file "~/org/recipes.org") "* %i%?")
        ("d" "Diary entry" entry (file "~/org/diary.org") "* %T %i%?")
        ("c" "Content" entry (file "~/org/content.org") "* %i%?")
        ("s" "Shopping" entry (file "~/org/shopping.org") "* %i%?")
        ("e" "Draft email" entry (file "~/org/draft_emails.org") "* %i%?")))
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '(("~/org/tasks.org" :level . 0)
                           ("~/org/politics.org" :level . 0)
                           ("~/org/biochem.org" :level . 0)
                           ("~/org/shopping.org" :level . 0)
                           ("~/org/draft_emails.org" :level . 0)
                           ("~/org/content.org" :level . 0)
                           ("~/org/recipes.org" :level . 0)
                           ("~/org/diary.org" :level . 0)))
;; youtube links (Prefix is 'yt')
(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))
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

(unless in-termux-p (require 'ox-bibtex))
(setq org-bibtex-file "/Users/tedgro/Dropbox/Reading/bibliography.bib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customisations

;; function that searches up for the nearest makefile and compiles it
(defun compile-parent (command)
  (interactive
   (let* ((make-directory (locate-dominating-file (buffer-file-name)
                                                  "Makefile"))
          (command (concat "make -k -C "
                           (shell-quote-argument make-directory))))
     (list (compilation-read-command command))))
  (compile command))

;; compile with C-c m
;; NB use C-u C-c m to change the compilation command
(global-set-key (kbd "C-c m") 'recompile)

;; quicker keystrokes
(setq echo-keystrokes 0.01)

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
(setq ispell-dictionary "uk")

;; misc getting rid of furniture
(tool-bar-mode -1)
(menu-bar-mode -1)
(unless in-termux-p (toggle-scroll-bar -1))

;; one-character confirm-or-deny
(fset 'yes-or-no-p 'y-or-n-p)

;; use ibuffer instead of buffer list
(define-key (current-global-map) [remap list-buffers] 'ibuffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "07c3a4ab1bc1fcae15baa26b9245ca9e11f4876ad6908df2219ec58d153058c0" "224b4c57e164d6ad2edc4ab1c2a20fbd95ad15e44f8fb2b797001cd39dd59123" default))
 '(package-selected-packages
   '(emacs-tron-theme tron-theme minimal-theme eval-in-repl eval-in-repl-python poly-R vterm modus-vivendi-theme modus-operandi-theme zenburn-theme which-key use-package try stan-snippets solarized-theme scala-mode pyenv-mode pdf-tools ox-tufte ox-pandoc ox-hugo org-tree-slide org-plus-contrib org-bullets ob-ipython nov neotree matlab-mode magit lsp-ui lsp-python latex-preview-pane key-chord jupyter julia-mode ivy-hydra ivy-bibtex inf-ruby htmlize helm-bibtex gruvbox-theme flycheck-stan faff-theme evil-org evil-collection ess elpy elfeed-org eldoc-stan ein dumb-jump dracula-theme doom-themes dockerfile-mode darktooth-theme csv-mode counsel-projectile company-stan commentary-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized c-eldoc auctex aggressive-indent ag ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
