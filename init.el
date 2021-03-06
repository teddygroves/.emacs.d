;; variable that checks if we're in termux
(defvar in-termux-p
  (and (equal (system-name) "localhost")
       (not (equal user-login-name "teddy")))
    "t if we are in termux emacs.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; https://github.com/daviwil/emacs-from-scratch/wiki/LSP-Python-(pyright)-config-in-emacs-from-scratch

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package 'org-plus-contrib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make sure environment variables are correct
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))


;; ** Performance

;; These options improve performance for lsp in emacs. Use ~M-x~ ~lsp-doctor~
;; in lsp-mode to investigate performance of your config.

;; check lsp documentation at
;; [[https://emacs-lsp.github.io/lsp-mode/page/performance/][lsp performace]]

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 MB
(setq read-process-output-max (* 1 1024 1024)) ;; 1 MB


;; ** No littering
;; Keep clean =~/.emacs.d= folder. 

;; Check ~lsp~ files in =~/.emacs.d/var/lsp/*=.

;; You can delete/modify this folder to hard reset lsp configuration in emacs.

;; [[https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org#keep-folders-clean][taken from EFS]] \\

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; * Evil

;; evil mode [[https://evil.readthedocs.io/en/latest/overview.html#installation-via-package-el][source]]

;; ** evil mode [[https://github.com/emacs-evil/evil][github repo]]

(use-package evil
  :ensure t
  :init
  (setq evil-shift-width 2)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-tree)
  :config

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'view-mode 'emacs)
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init 'compile)
  (evil-collection-init 'info)
  (evil-collection-init 'custom)
  (evil-collection-init 'dired)
  (evil-collection-init 'ivy)
  (evil-collection-init 'flycheck)
  (evil-collection-init 'xref)
  (evil-collection-init 'magit)
  (evil-collection-init 'which-key)
  )

;; ** evil-nerd-commentor

;; Use ~M-/~ for comment/uncomment.

;; [[https://github.com/redguardtoo/evil-nerd-commenter][source]]

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


;; ** undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  )

;; * Ivy, counsel

;; ** counsel

(use-package counsel
  :diminish ivy-mode
  :diminish counsel-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; ** ivy-misc [[https://github.com/Yevgnen/ivy-rich]]

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; ** prescient

(use-package ivy-prescient
  :after counsel
  :init
  (ivy-prescient-mode)
  (prescient-persist-mode)
  )

(use-package prescient
  :diminish
  :config
  )

;; * Treemacs

(use-package treemacs)

;; * Tools

;; ** which-key

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;; ** magit

;; [[https://magit.vc/][Magit]] is the best Git interface. Common Git
;; operations are easy to execute quickly using Magit’s command panel system.

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

;; ** projectile

;; [[https://docs.projectile.mx/projectile/index.html][Projectile]] is a
;; project management library for Emacs which makes it a lot easier to navigate
;; around code projects for various languages. Many packages integrate with
;; Projectile so it’s a good idea to have it installed even if you don’t use
;; its commands directly.

(use-package projectile
  :diminish projectile-mode
  :hook
  (after-init . projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-project-search-path '("~/Code" "~/Writing" "~/qmcm/projects"))
  (setq projectile-switch-project-action 'projectile-dired)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-dynamic-mode-line nil)
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  (projectile-track-known-projects-automatically nil))

;; (use-package counsel-projectile
  ;; :config (counsel-projectile-mode))

;; ** eldoc

(use-package eldoc
  :diminish eldoc-mode
  )


;; * Company

;; ** company-mode

  (use-package company
    :diminish company-mode
    :bind (:map company-active-map
                ("<tab>" . nil)
                ("TAB" . nil)
                ("M-<tab>" . company-complete-common-or-cycle)
                ("M-<tab>" . company-complete-selection))
    (:map lsp-mode-map
          ("M-<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 2)
    (company-idle-delay 0.01)
    :config
    )

;; ** prescient

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode)
  )

;; * Yasnippet

(use-package yasnippet-snippets)
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
)

;; * Flycheck

(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil)
  )

;; * Lsp mode

;; ** lsp-mode

;; [[https://github.com/daviwil/dotfiles/blob/master/Emacs.org#language-server-support][EFS
;; notes]] \\

;; Nice article about main features of emacs lsp-mode
;; ([[https://emacs-lsp.github.io/lsp-mode/page/main-features/][source)]] \\

;; EFS video
;; [[https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-08.org][notes]]\\

;; java specific lsp
;; [[https://github.com/neppramod/java_emacs/blob/master/emacs-configuration.org][setting]]
;; to learn how to setup lsp in emacs\\

;; Nice article to switch on/off certain features of lsp
;; ([[https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/][source)]]
;; \\

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook 
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-diagnostics-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-lens-enable nil)
  (lsp-disabled-clients '((python-mode . pyls)))
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config
  )

;; ** lsp-ivy [[https://github.com/emacs-lsp/lsp-ivy][source github]]\\

;; lsp-ivy integrates Ivy with lsp-mode to make it easy to search for things by
;; name in your code. When you run these commands, a prompt will appear in the
;; minibuffer allowing you to type part of the name of a symbol in your
;; code. Results will be populated in the minibuffer so that you can find what
;; you’re looking for and jump to that location in the code upon selecting the
;; result.\\

;; Try these commands with ~M-x~:\\

;;     ~lsp-ivy-workspace-symbol~ - Search for a symbol name in the current
;;     project workspace\\

;;     ~lsp-ivy-global-workspace-symbol~ - Search for a symbol name in all
;;     active project workspaces\\

(use-package lsp-ivy
  :after lsp-mode
  )

;; ** lsp-ui

;; Documentation: [[https://emacs-lsp.github.io/lsp-ui/]]

;; - ~lsp-ui-doc-focus-frame~ to enter the documentation frame to navigate and
;;   search around

;; - ~lsp-ui-doc-unfocus-frame~ to leave documentation frame

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  :config
  (setq lsp-ui-doc-position 'bottom)
  )

;; ** lsp-treemacs

;; Provides an even nicer UI on top of lsp-mode using Treemacs\\

;; - ~lsp-treemacs-symbols~ - Show a tree view of the symbols in the current
;;   file

;; - ~lsp-treemacs-references~ - Show a tree view for the references of the
;;   symbol under the cursor

;; - ~lsp-treemacs-error-list~ - Show a tree view for the diagnostic messages
;;   in the project

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  )


;; * Python configuration

;; [[https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-IDE-02.org][efs
;; series notes]]\\

;; [[https://ddavis.io/posts/emacs-python-lsp]]\\

;; some options are

;; - [[https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/][pyls]] Palantir

;; - [[https://emacs-lsp.github.io/lsp-python-ms][microsoft]] now depreciated by MS

;; - [[https://emacs-lsp.github.io/lsp-pyright][pyright]] also by Microsoft

;; ** pyright [[https://emacs-lsp.github.io/lsp-pyright/#configuration][config]] \\

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

;; ** pyvenv

;; Strongly recommend to use python virtualenv to python work properly in emacs.\\

;; Assuming venvs are installed here =~/.venvs=\\

;; Learn about setting python virtual env below\\

;; [[https://blog.fredrikmeyer.net/2020/08/26/emacs-python-venv.html]]\\

;; [[https://ddavis.io/posts/emacs-python-lsp]]\\

;; You can use ~M-x pyvenv-activate~ to activate specific venv \\

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  :config
  (pyvenv-mode t)
  )

  ;; Set correct Python interpreter
  ;; (setq pyvenv-post-activate-hooks
        ;; (list (lambda ()
                ;; (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  ;; (setq pyvenv-post-deactivate-hooks
        ;; (list (lambda ()
                ;; (setq python-shell-interpreter "python3")))))


;; ** formatting

(use-package blacken
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80)
  )

;; ** python-mode

(use-package python-mode
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  (python-mode . blacken-mode)
  (python-mode . yas-minor-mode)
  :custom
  (python-shell-interpreter "jupyter-console")
  (python-shell-interpreter-args "--simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  (python-shell-completion-native-enable nil)
  (python-indent-def-block-scale 1) ;; function arguments have normal indentation
  :bind
  ("<C-return>" . python-shell-send-statement)
  )

(use-package jupyter
  :unless in-termux-p
  :ensure t)

;; * Keybinding

;; Have a look at
;; [[https://www.masteringemacs.org/article/mastering-key-bindings-emacs][mastering
;; emacs]] tips for emacs keybinding.\\

;; ~C-c <LETTER>~ and ~F5-F9~ are meant for user bindings.\\

;; For package maintainers, ~C-c C-<ANY>~ or ~C-c <DIGIT>~ or ~C-c [{};:<>]~
;; are reserved for the major mode. Any other are reserved for minor modes,
;; e.g. ~C-c @~ in =outline-minor-mode=. \\

;; See ~(info "(elisp) Key Binding Conventions")~ for a more complete
;; explanation for package maintainers. You, as a user, can of course use any
;; key binding you like, but keep in mind that those bindings might conflict
;; with the ones chosen by the package maintainer.\\

;; ** General setup

;; we will use general package
;; ([[https://github.com/noctuid/general.el][source]]) for keybindings.

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer my/ctrl-c-keys
    :prefix "C-c")
  )

;; ** Global keys

;; use ~C-c~ prefix for global keybinding defined below
(my/ctrl-c-keys
  "t"  '(treemacs-select-window :which-key "treemacs-select")
  )

;; ** Lsp keybinding

;; use ~SPC~ prefix for ~lsp-mode~ keybinding defined below. These keybindings
;; are for ~evil~ normal mode.

(general-define-key
  :states '(normal visual)
  :keymaps 'lsp-mode-map
  :prefix "SPC"
   "d" '(lsp-find-definition :which-key "find-definitions")
   "r" '(lsp-find-references :which-key "find-references")
   "h" '(lsp-describe-thing-at-point :which-key "help-detailed")
   "e" '(lsp-ui-flycheck-list :which-key "flycheck-list")
   "o" 'counsel-imenu
   "x" 'lsp-execute-code-action)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes
(use-package tron-legacy-theme
  :ensure t
  :config
  (load-theme 'tron-legacy t)
  (setq tron-legacy-theme-vivid-cursor t)
  (setq tron-legacy-theme-dark-fg-bright-comments t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc Packages

(add-hook 'dired-mode-hook
          (lambda () (dired-hide-details-mode)))

(use-package dired-x ;; so that C-x C-j always does dired-jump
  :straight nil
  :demand t)
;; ace window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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

(use-package poly-org
  :ensure t)

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
(setq org-agenda-files (directory-files-recursively "~/qmcm/" "org$"))
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-custom-commands
      '(("c" "My custom agenda view" ((agenda) (tags-todo "*")))))

;; Gtd
(setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "QMCM Task" entry (file "~/qmcm/tasks.org") "* TODO %i%?")
        ("p" "Paper" entry (file "~/qmcm/reading.org") "* %i%?")
        ("b" "Biochem note" entry (file+headline "~/qmcm/biochem.org" "Notes") "* %i%?")
        ("r" "Recipe" entry (file "~/org/recipes.org") "* %i%?")
        ("d" "Diary entry" entry (file "~/org/diary.org") "* %T %i%?")
        ("c" "Content" entry (file "~/org/content.org") "* %i%?")
        ("s" "Shopping" entry (file "~/org/shopping.org") "* %i%?")
        ("z" "Programming tip" entry
         (file+headline "~/org/work/programming_tips.org" "Inbox") "** %i%?")
        ("e" "Draft email" entry (file "~/qmcm/draft_emails.org") "* %i%?")))
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '(("~/qmcm/tasks.org" :level . 0)
                           ("~/org/politics.org" :level . 0)
                           ("~/qmcm/biochem.org" :level . 0)
                           ("~/qmcm/papers.org" :level . 0)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customisations

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
 '(org-agenda-files
   '("~/qmcm/hani/hani.org" "~/qmcm/marina/marina.org" "~/qmcm/nick/nick.org" "~/qmcm/projects/apoptosis/notes.org" "~/qmcm/projects/clonal_variation/technical_report.org" "~/qmcm/projects/cobracon_slides/notes.org" "~/qmcm/projects/doe/doe.org" "~/qmcm/projects/pandas_slides/pandas.org" "~/qmcm/projects/performance/performance.org" "~/qmcm/projects/proteomics/proteomics.org" "~/qmcm/projects/thermodynamics/thermodynamics.org" "~/qmcm/projects/programming_tips.org" "~/qmcm/projects/readme.org" "~/qmcm/shannara/shannara.org" "~/qmcm/viktor/viktor.org" "~/qmcm/vishnu/vishnu.org" "~/qmcm/biochem.org" "~/qmcm/kinetics_meetings.org" "~/qmcm/notes.org" "~/qmcm/reading.org" "~/qmcm/tasks.org" "~/qmcm/weekly_slides.org" "~/qmcm/weekly_work_plans.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
