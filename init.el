;; Variable that checks if we're in termux
(defvar in-termux-p
  (and (equal (system-name) "localhost")
       (not (equal user-login-name "teddy")))
    "t if we are in termux emacs.")

;; explanation of how straight works and why you need to bootstrap it
;; https://systemcrafters.cc/advanced-package-management/using-straight-el/

;; a lot of this config is copied from here
;; https://github.com/daviwil/emacs-from-scratch/wiki/LSP-Python-(pyright)-config-in-emacs-from-scratch
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

;; (straight-use-package 'org-plus-contrib)
;; (straight-use-package '(org :local-repo nil))

;; org mode
;; todo: copy random bits of org config from below to here

(use-package citeproc
  :ensure t)

(use-package org
  :straight (:repo "https://git.savannah.gnu.org/git/emacs/org-mode.git")
  :after citeproc
  :ensure t
  :custom
  (org-cite-csl-styles-dir "~/Zotero/styles")
  (org-cite-csl-locales-dir "~/.emacs.d/straight/repos/org/etc/csl")
  (org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; org roam (for taking notes, not on termux for now)
(use-package org-roam
  :after org
  :unless in-termux-p
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/DropsyncFiles/org/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry  "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "Paper" plain "\n\n* Reference\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("w" "Website" plain "\n\n* Link\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n n" . org-roam-capture)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (add-to-list 'display-buffer-alist
              '("\\*org-roam\\*"
                (display-buffer-in-direction)
                (direction . right)
                (window-width . 0.33)
                (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make sure environment variables are correct
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package zmq
  :unless in-termux-p
  :ensure t)

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
  :after undo-tree
  :custom
  (evil-undo-system 'undo-tree)
  :init
  (setq evil-shift-width 2)
  (setq evil-want-integration t)
  :config
  (setq evil-want-keybinding nil)
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
  ;; (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init)
  :config
  (evil-collection-init 'compile)
  (evil-collection-init 'info)
  (evil-collection-init 'custom)
  (evil-collection-init 'dired)
  (evil-collection-init 'python)
  (evil-collection-init 'flycheck)
  (evil-collection-init 'xref)
  (evil-collection-init 'magit)
  (evil-collection-init 'which-key)
  (evil-collection-init 'which-key)
  (unless in-termux-p (evil-collection-init 'mu4e))
  )

(use-package evil-org
  :ensure t
  :after evil org
  :init
  (add-hook 'org-mode-hook #'evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; ** evil-nerd-commentor

;; Use ~M-/~ for comment/uncomment.

;; [[https://github.com/redguardtoo/evil-nerd-commenter][source]]

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


;; ** undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minibuffer

;; mini-frame https://github.com/muffinmad/emacs-mini-frame
;; It puts the minibuffer in the middle of the screen
;; (use-package mini-frame
;;   :ensure t
;;   :init (mini-frame-mode)
;;   :custom
;;   (mini-frame-show-parameters
;;    '((top . 100)
;;      (width . 0.7)
;;      (left . 0.5)
;;      (height . 15))))

;; vertico 
;; https://github.com/minad/vertico
;; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; vertico-directory makes directory search in minibuffer a bit nicer
(use-package vertico-directory
  :straight (:local-repo "vertico/extensions")
  :ensure t
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :demand t
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override
              #'consult-completing-read-multiple)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package embark
  :straight (:type git :host github :repo "oantolin/embark")
  :demand t
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list
           'display-buffer-alist
           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
             (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(setq )

;; bibtex actions
(defvar my/bibs '("/Users/tedgro/Reading/bibliography.bib"))

(use-package bibtex-completion
  :demand t
  :ensure t
  :config
  (setq bibtex-completion-additional-search-fields '(doi url)
        bibtex-completion-pdf-extension ".pdf"
        bibtex-completion-bibliography '("/Users/tedgro/Reading/bibliography.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-open-function
        (lambda (fpath) (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))))

(use-package parsebib
  :demand t
  :straight (:type git :host github :repo "joostkremers/parsebib")
  :ensure t)

(use-package oc-bibtex-actions
  :ensure t
  :demand t
  :straight (:type git :host github :repo "bdarcus/bibtex-actions")
  :bind (("C-c b" . org-cite-insert)
         ("M-o" . org-open-at-point)
         :map minibuffer-local-map
         ("M-b" . bibtex-actions-insert-preset))
  :after (embark oc citeproc)
  :config
  (setq bibtex-actions-bibliography my/bibs
        org-cite-global-bibliography my/bibs
        org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions
        org-cite-activate-processor 'basic))

;; Use consult-completing-read for enhanced interface.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)


;; (use-package bibtex-actions
;;   :ensure t
;;   :demand t
;;   :straight (:type git :host github :repo "bdarcus/bibtex-actions"
;;                    ;; :branch "file-field"
;;                    )
;;   :bind (("C-c b" . bibtex-actions-insert-citation)
;;          :map minibuffer-local-map
;;          ("M-b" . bibtex-actions-insert-preset))
;;   :after (embark parsebib bibtex-completion)
;;   :config
;;   (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
;;   (add-to-list 'embark-keymap-alist '(bib-reference . bibtex-actions-map))
;;   (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))
;;   (advice-add #'completing-read-multiple
;;               :override #'consult-completing-read-multiple)
;;   (setq bibtex-actions-file-variable "file"
;;         bibtex-actions-bibliography '("/Users/tedgro/Reading/bibliography.bib")))

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
  (setq projectile-project-search-path '("~/Code" "~/Writing" "~/dtu/projects"))
  ;; (setq projectile-switch-project-action 'projectile-dired)
  :custom
  ;; (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-find-file)
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

;; * Flycheck

(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-check-syntax-automatically '(idle-change save)
        flycheck-highlighting-mode 'symbols
        flycheck-deferred-syntax-check nil)
  :config
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-flake8 'python-pyright)
  )

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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold 100000000) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1mb https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq lsp-log-io nil)
  (setq lsp-idle-delay 0.500)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'"))
  (setq lsp-file-watch-threshold 3000)
  :config (setq lsp-headerline-breadcrumb-enable nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)


;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; lsp-related packages

;; (use-package lsp-ui
  ;; :after lsp-mode
  ;; :commands lsp-ui-mode)

;; (use-package treemacs)

;; (use-package lsp-treemacs :after (lsp-mode treemacs) :commands lsp-treemacs-errors-list)

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :custom (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-venv-path "/Users/tedgro/.venvs")
  (lsp-pyright-venv-directory "/Users/tedgro/.venvs")
  (lsp-pyright-multi-root nil)
  (lsp-pyright-stub-path "/Users/tedgro/Code/cloned/python-type-stubs")
  )


(use-package company
  :diminish company-mode
  :bind
  (:map company-active-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("M-<tab>" . company-complete-common-or-cycle)
        ("M-<tab>" . company-complete-selection))
  ;; (:map lsp-mode-map ("M-<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.01)
  :config
  )

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer my/ctrl-c-keys
    :prefix "C-c")

  (my/ctrl-c-keys "t"  '(treemacs-select-window :which-key "treemacs-select")))

(general-define-key
  :states '(normal visual)
  ;; :keymaps 'lsp-mode-map
  :prefix "SPC"
   "d" '(lsp-find-definition :which-key "find-definitions")
   "r" '(lsp-find-references :which-key "find-references")
   "h" '(lsp-describe-thing-at-point :which-key "help-detailed")
   "e" '(lsp-ui-flycheck-list :which-key "flycheck-list")
   "SPC" 'python-shell-send-statement
   ;; "o" 'counsel-imenu
   "x" 'lsp-execute-code-action)



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

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil))

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

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/jupyter-console")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "jupyter-console")))))


;; ** formatting
(use-package py-isort
  :ensure t
  :init
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package blacken
  :init
  (setq-default blacken-fast-unsafe t)
  ;; (setq-default blacken-line-length 80)
  ;; (setq-default blacken-allow-py36 t)
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
  (py-closing-list-dedents-bos t)  ;; list parentheses have inline indentation
  :bind
  ("<C-return>" . python-shell-send-statement)
  )

;; (use-package jupyter
  ;; :unless in-termux-p
  ;; :ensure t)

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
(use-package mu4e-alert
  :unless in-termux-p
  :init
  (setq mu4e-alert-interesting-mail-query
        "flag:unread AND NOT flag:trashed AND (maildir:/Gmail/Inbox OR maildir:/dtu/Inbox)")
  :config
  (mu4e-alert-disable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mode line

;; nice clock format
(setq display-time-format "W%W %d%b %k:%M"
      display-time-default-load-average nil)
(display-time-mode 1)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!
(use-package doom-modeline
  :after eshell doom-themes     ;; load after eshell and doom theme
  :custom-face
  ;; (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-mu4e (if in-termux-p nil t))
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil)
  :init
  (setq doom-modeline-percent-position '(-3 ""))
  (doom-modeline-mode 1)
  :config
  (unless in-termux-p
      (mu4e-alert-enable-mode-line-display))
  ;; (setq-default header-line-format mode-line-format)
  ;; (setq-default mode-line-format nil)
  )

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
  (vterm-buffer-name-string "vterm %s"))

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
;;   :hook (stan-mode . eldoc-stan-setup)
;;   ;;
;;   :config
;;   ;; No configuration options as of now.
;;   )

;; ;;; stan-snippets.el
;; (use-package stan-snippets
;;   ;; Uncomment if directly loading from your development repo
;;   ;; :load-path "your-path/stan-mode/stan-snippets/"
;;   :hook (stan-mode . stan-snippets-initialize)
;;   ;;
;;   :config
;;   ;; No configuration options as of now.
;;   )


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org mode configuration

;; Use recent version of org mode with contributed packages
;; (unless (package-installed-p 'org-plus-contrib)  ;; Make sure the Org package is
;;(package-install 'org-plus-contrib))

;; interface
(setq org-src-fontify-natively t)
(setq org-indent_mode nil)
(setq org-adapt-indentation nil)

;; export
;; (use-package ox-pandoc
;;   :unless in-termux-p
;;   :ensure t
;;   :init (add-to-list 'exec-path "/usr/local/bin"))
;; (require 'ox-publish)
;; (defvar org-export-output-directory-prefix  ;; Export to export_<file_type>
;;   "export_"
;;   "prefix of directory used for org-mode export")
;; (defadvice org-export-output-file-name (before org-add-export-dir activate)
;;   "Modifies org-export to place exported files in a different directory"
;;   (when (not pub-dir)
;;     (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
;;     (when (not (file-directory-p pub-dir))
;;       (make-directory pub-dir))))

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

(use-package ob-async
  :ensure t
  :unless in-termux-p)

(use-package jupyter
  :ensure t
  :unless in-termux-p)

(unless in-termux-p
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (python . t)
     (shell . t)
     (jupyter . t))
   )
  ;; Syntax highlight in #+BEGIN_SRC blocks
  (setq org-src-fontify-natively t)
  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
)

;; (setq org-confirm-babel-evaluate nil)
;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Org agenda
(setq dropsync-dir-parent (if in-termux-p "~/storage/shared/" "~/Dropbox/"))
(setq dropsync-dir (concat dropsync-dir-parent "DropsyncFiles/"))
(setq org-dir (concat dropsync-dir "org/"))
(setq todo-file (concat org-dir "tasks.org"))
(setq dtu-notes-file (concat org-dir "dtu.org"))
(setq draft-file (concat org-dir "draft.org"))

(setq org-agenda-window-setup 'only-window)  ;; agenda is full screen
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-custom-commands
      '(("w" "Work view"
         ((agenda)
          (tags-todo "admin")
          (tags-todo "reading")
          (tags-todo "-admin-reading"))
         ((org-agenda-filter-preset '("+work"))))
        ("l" "Life view"
         ((agenda) (todo))
         ((org-agenda-filter-preset '("+life"))))))
(setq org-agenda-files (file-expand-wildcards (concat org-dir "*.org")))
(setq org-agenda-prefix-format " %i %?-12t% s")
(setq org-agenda-hide-tags-regexp "work")

;; org todo
(setq org-todo-keywords '((sequence "TODO" "|" "DONE")))

;; org capture
(define-key global-map "\C-cc" 'org-capture)
(add-hook 'org-capture-mode-hook #'org-align-all-tags)
(setq org-capture-templates
      '(("w" "Work todo" entry (file todo-file) "* TODO %i%? :work:\n%T")
        ("l" "Life todo" entry (file todo-file) "* TODO %i%? :life:\n%T")
        ("e" "Email" entry (file todo-file) "* TODO %a")
        ("t" "Misc todo" entry (file todo-file) "* TODO %i%?\n%T")
        ("d" "Draft" entry (file draft-file) "* %i%?\n%T")))

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

;; (unless in-termux-p (require 'ox-bibtex))
;; (setq org-bibtex-file "/Users/tedgro/Dropbox/Reading/bibliography.bib")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRAMP
(setq tramp-default-method "ssh")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orderless
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package olivetti
 :ensure t
 :custom
 (olivetti-body-width (if in-termux-p 60 80))
 :config
 (use-package olivetti :ensure t)
 (require 'olivetti)
 (define-global-minor-mode global-olivetti-mode olivetti-mode
   (lambda ()
     (unless (memq major-mode '(minibuffer-mode which-key-mode mu4e-headers-mode))
       (olivetti-mode 1))))
(if in-termux-p (setq-default fringe-mode nil))
 (global-olivetti-mode 1)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; soccer
(use-package soccer
  :straight (soccer :type git :host github :repo "md-arif-shaikh/soccer")

  :custom
  (soccer-time-local-time-utc-offset "+0300"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Email
(unless in-termux-p
(use-package mu4e
  :unless in-termux-p
  :straight (:local-repo "/usr/local/share/emacs/site-lisp/mu/mu4e/"
             :pre-build ())
  :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; message on right hand side:
  ;; see https://www.djcbsoftware.nl/code/mu/mu4e/Split-view.html
  ;; (setq mu4e-split-view 'vertical)
  (setq mu4e-headers-visible-columns 100)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; searches that return newsletters
  (setq tg/news_queries
   '("from:plura-list@pluralistic.net"          ;; Plura-list
     "from:huw@substack.com"                    ;; Utopian drivel
     "list:tl_1154865.tinyletter.com"           ;; Time for some mage theory
     "list:tl_1405305.tinyletter.com"           ;; Michael Betancourt
     "list:3983822d8f.193801.list-id.mcsv.net"                 ;; Cortado
     "list:e39e7b4c29a565f8cbebeb66a.384876.list-id.mcsv.net"  ;; Olly
     "list:24810bc6de7dd14c076735f2d.688814.list-id.mcsv.net"  ;; souldren
     "list:514009622.xt.local"                  ;; Giddy
     "from:office@exlibris.berlin"))            ;; Ex libris

  ;; Don't show related messages
  (setq mu4e-headers-include-related nil)

  ;;
  (defun tg/mu4e-narrow-unread ()
    "Narrow search to unread"
    (interactive)
    (mu4e-headers-search-narrow "flag:unread"))
  (define-key mu4e-headers-mode-map (kbd "M-u") 'tg/mu4e-narrow-unread)

  ;; bookmarks
  (setq mu4e-bookmarks
    '((:name  "Unread messages"
       :query "flag:unread AND NOT flag:trashed AND (maildir:/Gmail/Inbox OR maildir:/dtu/Inbox)"
       :key ?u)
      (:name "Today's messages"
       :query "date:today..now AND (maildir:/Gmail/Inbox OR maildir:/dtu/Inbox)"
       :key ?t)
      (:name "Last 7 days"
       :query "date:7d..now AND NOT maildir:\"/Gmail/[Google mail]/All Mail\ AND NOT maildir:\"/Gmail/[Google mail]/Sent Mail\""
        :key ?w)
      (:name  "Note to self"
       :query "from:groves.teddy@gmail.com AND maildir:\"/Gmail/Inbox\""
       :key   ?n)
      (:name  "OpenTECR mailing list"
       :query "list:opentecr.googlegroups.com"
       :key   ?o)
      (:name  "Newsletters"
       :query (string-join tg/news_queries " OR ")
       :key   ?l)
      (:name  "Github"
       :query "from:notifications@github.com"
       :key   ?g)
      (:name  "Discourse"
       :query "from:mc_stan@discoursemail.com"
       :key   ?d)))

  ;; date format in headers view
  (setq mu4e-headers-date-format "%Y-%m-%d")

  ;; capture emails
  (define-key mu4e-view-mode-map    (kbd "C-c c") 'mu4e-org-store-and-capture)
  (define-key mu4e-headers-mode-map    (kbd "C-c c") 'mu4e-org-store-and-capture)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))

  ;; isync setup
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;; slightly nicer rendering of html emails
  (setq mu4e-html2text-command "html2text -utf8 -width 72")

  ;; set up msmtp
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program (executable-find "msmtp"))
  (setq message-sendmail-envelope-from 'header)

  ;; nicer comosing
  (setq mu4e-compose-format-flowed t)

  ;; no confirmation before quit
  (setq mu4e-confirm-quit nil)

  ;; cc and bcc fields
  (add-hook 'mu4e-compose-mode-hook
            (defun timu/add-cc-and-bcc ()
              "My Function to automatically add Cc & Bcc: headers."
              (save-excursion (message-add-header "Cc:\n"))
              (save-excursion (message-add-header "Bcc:\n"))))

  ;; contexts (need one of these for work email maybe someday)
  (setq mu4e-contexts
        (list
         ;; Personal account
         (make-mu4e-context
          :name "Gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "groves.teddy@gmail.com")
                    (user-full-name    . "Teddy Groves")
                    (mu4e-compose-signature . "Teddy via Gmail")
                    (mu4e-drafts-folder  . "/Gmail/[Google mail]/Drafts")
                    (mu4e-sent-folder  . "/Gmail/[Google mail]/Sent Mail")
                    (mu4e-refile-folder  . "/Gmail/[Google mail]/All Mail")
                    (mu4e-trash-folder  . "/Gmail/[Google mail]/Trash")))
         (make-mu4e-context
          :name "dtu"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/dtu" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "tedgro@dtu.dk")
                    (user-full-name    . "Teddy Groves")
                    (mu4e-compose-signature . "Teddy Groves via DTU email")
                    (mu4e-drafts-folder  . "/dtu/Drafts")
                    (mu4e-sent-folder  . "/dtu/Sent Items")
                    (mu4e-refile-folder  . "/dtu/Archive")
                    (mu4e-trash-folder  . "/dtu/Deleted Items")))
         ))

  (setq mu4e-maildir-shortcuts
    '((:maildir "/Gmail/Inbox"    :key ?i)
      (:maildir "/Gmail/[Google mail]/Sent Mail" :key ?s)
      (:maildir "/Gmail/[Google mail]/Trash"     :key ?t)
      (:maildir "/Gmail/[Google mail]/Drafts"    :key ?d)
      (:maildir "/Gmail/[Google mail]/All Mail"  :key ?a)
      (:maildir "/dtu/Inbox"  :key ?I)
      (:maildir "/dtu/Drafts"    :key ?D)

      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customisations

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

;; fix mac meta and super keys
(setq-default mac-option-modifier 'meta)
(setq-default mac-command-modifier 'super)
(global-set-key (kbd "s-k") 'kill-this-buffer)


;; font
(mac-auto-operator-composition-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 )
