;; -*- lexical-binding: t; -*-

;; bootstrap stuff and stratight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-repository-branch "develop")

;;;; No littering
(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; markdown mode
(use-package markdown-mode
  :ensure t)

;; pdf-tools
(use-package pdf-tools
  :after evil
  :ensure t
  :bind (:map pdf-view-mode-map ("B" . pdf-history-backward))
  :custom
  (pdf-view-use-scaling t)
  :init
  (pdf-tools-install)
  (pdf-loader-install))

(use-package citeproc
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make sure environment variables are correct
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package zmq
  ;; :unless in-termux-p
  :ensure t)


;; ** undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package yasnippet-snippets
  :ensure t)


(use-package parsebib
  :demand t
  :straight (:type git :host github :repo "joostkremers/parsebib")
  :ensure t)


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

;; customize project.el
(use-package project
  :custom
  (project-switch-commands 'project-dired))

;; ** projectile

;; [[https://docs.projectile.mx/projectile/index.html][Projectile]] is a
;; project management library for Emacs which makes it a lot easier to navigate
;; around code projects for various languages. Many packages integrate with
;; Projectile so it’s a good idea to have it installed even if you don’t use
;; its commands directly.

;; (use-package projectile
;;   :diminish projectile-mode
;;   :hook
;;   (after-init . projectile-mode)
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (setq projectile-project-search-path '("~/Code" "~/Writing" "~/dtu/projects"))
;;   ;; (setq projectile-switch-project-action 'projectile-dired)
;;   :custom
;;   ;; (projectile-completion-system 'ivy)
;;   (projectile-switch-project-action 'projectile-find-file)
;;   (projectile-dynamic-mode-line nil)
;;   (projectile-enable-caching t)
;;   (projectile-indexing-method 'hybrid)
;;   (projectile-track-known-projects-automatically nil))

;; (use-package counsel-projectile
  ;; :config (counsel-projectile-mode))

;; ** eldoc

(use-package eldoc
  :diminish eldoc-mode
  )


;; ** prescient

(use-package prescient
  :config
  (prescient-persist-mode))

;; * Yasnippet

(use-package yasnippet-snippets)
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'org-mode-hook #'yas-minor-mode)
)

;; Flymake

(use-package flymake
  :ensure t)
;; * Flycheck

;; (use-package flycheck
  ;; :diminish flycheck-mode
  ;; :init
  ;; (setq flycheck-idle-change-delay 5.0
        ;; flycheck-display-errors-delay 0.9
        ;; flycheck-check-syntax-automatically '(idle-change save)
        ;; flycheck-highlighting-mode 'symbols
        ;; flycheck-deferred-syntax-check nil)
  ;; :config
  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)
  ;; (flycheck-add-next-checker 'python-flake8 'python-pyright)
  ;; )

;; ocaml
(use-package tuareg
  :ensure t)
;; (use-package merlin
  ;; :after lsp-mode
  ;; :ensure t
  ;; :config
  ;; (add-hook 'tuareg-mode-hook #'merlin-mode)
  ;; (require 'merlin-company))

;; * Lsp mode

;; (use-package lsp-mode
;;   :commands lsp lsp-deferred
;;   :after orderless
;;   :custom
;;   (lsp-completion-provider :none) ;; we use Corfu!
;;   (lsp-eldoc-enable-hover nil)
;;   (lsp-trigger-auto-activate nil)
;;   (lsp-file-watch-threshold 3000)
;;   (lsp-idle-delay 0.500)
;;   (lsp-log-io nil)
;;   (lsp-headerline-breadcrumb-enable nil)
;;   (lsp-keep-workspace-alive nil)
;;   :init
;;   (defun tg/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   (setq gc-cons-threshold 100000000) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (with-eval-after-load 'lsp-mode
;;     (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'"))
;;   :hook
;;   ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;   (python-mode . lsp-deferred)
;;   ;; if you want which-key integration
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   (lsp-completion-mode . tg/lsp-mode-setup-completion))

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-ensure))
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (push #'flymake-eldoc-function eldoc-documentation-functions)
              ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose) ;optional
              )))

;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; lsp-related packages

;; (use-package lsp-ui
  ;; :after lsp-mode
  ;; :commands lsp-ui-mode)

;; (use-package treemacs)

;; (use-package lsp-treemacs :after (lsp-mode treemacs) :commands lsp-treemacs-errors-list)
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
;;   :custom
;;   (lsp-pyright-venv-path "/Users/tedgro/.venvs")
;;   (lsp-pyright-venv-directory "/Users/tedgro/.venvs")
;;   (lsp-pyright-auto-import-completions t)
;;   (lsp-pyright-multi-root nil)
;;   (lsp-pyright-stub-path "/Users/tedgro/Code/stubs/"))


;; (use-package company
;;   :diminish company-mode
;;   :bind
;;   (:map company-active-map
;;         ("<tab>" . nil)
;;         ("TAB" . nil)
;;         ("M-<tab>" . company-complete-common-or-cycle)
;;         ("M-<tab>" . company-complete-selection))
;;   ;; (:map lsp-mode-map ("M-<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 2)
;;   (company-idle-delay 0.01)
;;   :config
;;   )

(use-package general
  :after evil-collection
  :config
  (general-evil-setup t)
  (general-create-definer my/ctrl-c-keys
    :prefix "C-c")
  (my/ctrl-c-keys "t"  '(treemacs-select-window :which-key "treemacs-select"))
  (general-define-key
    :states '(normal visual)
    ;; :keymaps 'lsp-mode-map
    :prefix "SPC"
    "d" '(xref-find-definitions :which-key "find-definitions")
    "r" '(xref-find-references :which-key "find-references")
    "f" '(eglot-format :which-key "eglot-format")
    "h" '(eldoc :which-key "eldoc")
    "i" '(consult-imenu :which-key "consult-imenu")
    "~" '(consult-lsp-symbols :which-key "consult-lsp-symbols")
    "e" '(consult-flymake :which-key "consult-flymake")
    "E" '(flymake-show-project-diagnostics :which-key "project diagnostics")
    "b" '(consult-buffer :which-key "consult-buffer")
    "g" '(consult-ripgrep :which-key "consult-ripgrep")
    "y" '(consult-yasnippet :which-key "consult-yasnippet")
    "p" '(consult-project-extra-find :which-key "consult-project-extra-find")
    "s" '(consult-flyspell :which-key "consult-flyspell")
    ;; "SPC" 'python-shell-send-statement
    ;; "o" 'counsel-imenu
    "x" 'lsp-execute-code-action))



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


;; ** lsp-ui

;; Documentation: [[https://emacs-lsp.github.io/lsp-ui/]]

;; - ~lsp-ui-doc-focus-frame~ to enter the documentation frame to navigate and
;;   search around

;; - ~lsp-ui-doc-unfocus-frame~ to leave documentation frame

;; (use-package lsp-ui
  ;; :hook (lsp-mode . lsp-ui-mode)
  ;; :after lsp-mode
  ;; :custom
  ;; (lsp-ui-doc-enable nil))

;; ** lsp-treemacs

;; Provides an even nicer UI on top of lsp-mode using Treemacs\\

;; - ~lsp-treemacs-symbols~ - Show a tree view of the symbols in the current
;;   file

;; - ~lsp-treemacs-references~ - Show a tree view for the references of the
;;   symbol under the cursor

;; - ~lsp-treemacs-error-list~ - Show a tree view for the diagnostic messages
;;   in the project

;; (use-package lsp-treemacs
  ;; :after (lsp-mode treemacs)
  ;; )


;; * Python configuration

;; [[https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-IDE-02.org][efs
;; series notes]]\\

;; [[https://ddavis.io/posts/emacs-python-lsp]]\\

;; some options are

;; - [[https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/][pyls]] Palantir

;; - [[https://emacs-lsp.github.io/lsp-python-ms][microsoft]] now depreciated by MS

;; - [[https://emacs-lsp.github.io/lsp-pyright][pyright]] also by Microsoft

;; ** pyright [[https://emacs-lsp.github.io/lsp-pyright/#configuration][config]] \\


;; ** pyvenv

;; Strongly recommend to use python virtualenv to python work properly in emacs.\\

;; Assuming venvs are installed here =~/.venvs=\\

;; Learn about setting python virtual env below\\

;; [[https://blog.fredrikmeyer.net/2020/08/26/emacs-python-venv.html]]\\

;; [[https://ddavis.io/posts/emacs-python-lsp]]\\

;; You can use ~M-x pyvenv-activate~ to activate specific venv \\

(use-package pydoc
  :ensure t
  :bind (:map python-mode-map ("C-c d" . 'pydoc-at-point)))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env "bin/ipython")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "ipython"))))
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))


;; ** formatting
;; (use-package py-isort
  ;; :ensure t
  ;; :init
  ;; (add-hook 'before-save-hook 'py-isort-before-save))

;; (use-package blacken
  ;; :init
  ;; (setq-default blacken-fast-unsafe t)
  ;; (setq-default blacken-allow-py36 t)
  ;; )

;; ** python-mode

(use-package python-mode
  :hook
  (python-mode . pyvenv-mode)
  ;; (python-mode . flycheck-mode)
  ;; (python-mode . blacken-mode)
  (python-mode . yas-minor-mode)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt --pprint")
  (python-shell-setup-codes '("import sys; sys.argv = sys.argv[:1]"))
  ;; (python-shell-prompt-detect-failure-warning nil)
  ;; (python-shell-completion-native-enable t)
  (python-indent-def-block-scale 1) ;; function arguments have normal indentation
  (py-closing-list-dedents-bos t)  ;; list parentheses have inline indentation
  :bind
  ("<C-return>" . python-shell-send-statement))

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


;; ** Global keys

;; use ~C-c~ prefix for global keybinding defined below

;; ** Lsp keybinding

;; use ~SPC~ prefix for ~lsp-mode~ keybinding defined below. These keybindings
;; are for ~evil~ normal mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes
(use-package modus-themes
  :ensure 
  :init
  (setq modus-themes-mode-line '(accented borderless (padding . 4))
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-fringes nil
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold intense)
        modus-themes-completions 'opinionated
        modus-themes-org-blocks 'tinted-background
        modus-themes-region '(bg-only)
        modus-themes-headings
        '((1 . (rainbow background bold 1))
          (2 . (rainbow bold 1))
          (3 . (rainbow bold 1))
          (t . (semilight 1))))
  (modus-themes-load-themes)

  :bind ("<f5>" . modus-themes-toggle)
  :config (load-theme 'modus-operandi t))

;; (use-package doom-themes
;;   ;; :unless in-termux-p
;;   :after org
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-palenight t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   ;; (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   ;; (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package nano-modeline
  ;; :demand t
  ;; :ensure t
  ;; :config (nano-modeline-mode)
  ;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mode line

(setq mode-line-position-column-line-format '("%l:%c"))
;; position
(setq mode-line-percent-position nil)
(setq line-number-mode 1
      column-number-mode 1)
;; (setq column-number-mode nil)
;; don't show any mode info in the mode line
(setq mode-line-modes nil)
;; battery status
;; (setq battery-mode-line-format "Battery:[%b%p%%]")
;; (setq display-battery-mode 1)
;; clock
(setq display-time-mode nil)
;; (setq display-time-format " [W%W|%d%b|%k:%M]"
;;       display-time-default-load-average nil)
;;       display-time-mode 1
;;       global-mode-string (remove 'display-time-string global-mode-string))
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; mode-line-mule-info
                ;; mode-line-client
                mode-line-modified
                ;; mode-line-remote
                ;; mode-line-frame-identification
                mode-line-position
                evil-mode-line-tag
                mode-line-buffer-identification
                ;; (vc-mode vc-mode)
                ;; "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc Packages

;; dired customisation
(when (string= system-type "darwin")  ;; don't use ls-dired in macos
  (setq dired-use-ls-dired nil))

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
  :ensure t
  :custom
  (vterm-install t)
  (vterm-buffer-name-string "vterm %s"))

;; yaml

(use-package yaml-mode
  :ensure t)

;; Stan
(use-package stan-mode
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :ensure t
  :hook (stan-mode . stan-mode-setup)
  ;;
  :custom
  ;; The officially recommended offset is 2.
  (stan-indentation-offset 2))

;; (use-package flycheck-stan
  ;; :ensure t
  ;; :after stan-mode
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  ;; :hook ((stan-mode . flycheck-stan-stanc3-setup))
  ;; :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  ;; (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  ;; (setq flycheck-stanc3-executable
        ;; "/Users/tedgro/.cmdstan/cmdstan-2.28.1/bin/stanc"))

;; ;;; company-stan.el
;; (use-package company-stan
;;   ;; Uncomment if directly loading from your development repo
;;   ;; :load-path "your-path/stan-mode/company-stan/"
;;   :hook (stan-mode . company-stan-setup)
;;   ;;
;;   :config
;;   ;; Whether to use fuzzy matching in `company-stan'
;;   (setq company-stan-fuzzy 1))

;; ;;; eldoc-stan.el
;; (use-package eldoc-stan
;;   ;; Uncomment if directly loading from your development repo
;;   ;; :load-path "your-path/stan-mode/eldoc-stan/"
;;   :ensure t
;;   :hook (stan-mode . eldoc-stan-setup)
;;   ;;
;;   :config
;;   ;; No configuration options as of now.
;;   )

;;; stan-snippets.el
(use-package stan-snippets
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-snippets/"
  :hook
  (stan-mode . stan-snippets-initialize)
  (stan-mode . yas-minor-mode)
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
  :init (add-hook 'csv-mode-hook (lambda () (font-lock-mode -1)))
  :config
  (setq csv-separators '("," ";", ":"))
  (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

;; R
(use-package ess
  :ensure t
  :init (require 'ess-site)
  :config
  (exec-path-from-shell-copy-env "LC_ALL")
  (exec-path-from-shell-copy-env "LANG"))

;; jupyter
(use-package jupyter
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRAMP
(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; olivetti
(use-package olivetti
 :ensure t)

;;;;;;;;;;;;;;;;;;; accounting
(use-package hledger-mode
  :ensure t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :bind (("C-c j" . hledger-run-command))
  :custom
  (hledger-jfile (concat tg/icloud-drive "Documents/Finance/2022.journal")))

;;;;;;;;;;;;;;;;;;;;;;;;; fonts
(use-package all-the-icons
  :if (display-graphic-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Spell checking with aspell
(use-package flyspell
  :ensure t
  :custom
  (ispell-program-name "aspell")
  (aspell-dictionary "en_GB-ise-wo_accents")
  (aspell-program-name "/opt/homebrew/bin/aspell")
  (ispell-dictionary "en_GB-ise-wo_accents")
  (ispell-program-name "/opt/homebrew/bin/aspell")
  :config
  (define-key flyspell-mode-map [down-mouse-4] 'flyspell-correct-word)
  (add-hook 'org-mode-hook 'flyspell-mode))

;; (use-package flyspell-correct
  ;; :ensure t
  ;; :bind
  ;; ("C-M-;" . flyspell-correct-wrapper)
  ;; ("C-M-]" . flyspell-correct-next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; winner mode: return to previous window configuration with C-c <left>
(use-package winner
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org mode
(load-file "~/.emacs.d/tg/org-mode-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org roam
(load-file "~/.emacs.d/tg/org-roam-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; evil mode
(load-file "~/.emacs.d/tg/evil-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; References
(load-file "~/.emacs.d/tg/reference-handling.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Latex
(load-file "~/.emacs.d/tg/latex-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Completion
(load-file "~/.emacs.d/tg/completion.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elfeed
(load-file "~/.emacs.d/tg/elfeed-config.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Email
;; (load-file "~/.emacs.d/tg/email.el")

;;;;;;;;;;;;;;;;;;;;;;;;; fun packages
(load-file "~/.emacs.d/tg/fun-packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customisations

;; use fixed width fonts everywhere
(setq shr-use-fonts nil)

;; don't truncate lines in html converted text
(setq shr-max-width nil
      shr-width nil)

;; right option key behaves as normal, for umlauts etc
(setq ns-right-alternate-modifier 'none)

;; (add-to-list 'auto-mode-alist '("\\.stan\\'" . c++-mode))

;; modeline at top
;; (setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format nil)

;; no cursor in unselected windows
(setq-default cursor-in-non-selected-windows nil)

;; margins
(setq-default fringes-outside-margins t)
        (setq-default left-margin-width 1)
        (setq-default right-margin-width 1)

;; don't show loads of warnings
(setq warning-minimum-level :error)

;; quicker keystrokes
(setq echo-keystrokes 0.01)

;; Don't report auto-reverts
(setq auto-revert-verbose nil)

;; Default fill column 80
(setq-default fill-column 80)

;; Autosaves to temporary directory
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

;; images
(auto-image-file-mode 1)
(add-hook 'image-mode-hook 'auto-revert-mode)

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

;; save place in file
(save-place-mode 1)

;; no dialog boxes
(setq use-dialog-box nil)

;; auto revert file buffers
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; misc getting rid of furniture
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; one-character confirm-or-deny
(fset 'yes-or-no-p 'y-or-n-p)

;; fix mac meta and super keys
(setq-default mac-option-modifier 'meta)
(setq-default mac-left-option-modifier 'meta)
(setq-default mac-right-option-modifier 'meta)
(setq-default mac-command-modifier 'super)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s-v") 'yank)
; Allow hash to be entered  
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))


;; font
;; (mac-auto-operator-composition-mode t)
