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

;; auctex
(use-package auctex
  :defer t
  :ensure t)


;;; evil mode and friends
(use-package evil
  :ensure t
  :after undo-tree
  :custom
  (evil-undo-system 'undo-tree)
  :init
  (setq evil-shift-width 2)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; tab does org-cycle in normal mode
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
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
  ;; :custom (evil-collection-company-use-tng nil)
  :config
  (with-eval-after-load 'info (evil-collection-info-setup))
  (with-eval-after-load 'custom (evil-collection-custom-setup))
  (with-eval-after-load 'dired (evil-collection-dired-setup))
  (with-eval-after-load 'python (evil-collection-python-setup))
  (with-eval-after-load 'flycheck (evil-collection-flycheck-setup))
  (with-eval-after-load 'magit (evil-collection-magit-setup))
  (with-eval-after-load 'which-key (evil-collection-which-key-setup))
  (with-eval-after-load 'mu4e (evil-collection-mu4e-setup)))

;; evil org mode, but just for the agenda
(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; ** evil-nerd-commentor

;; Use ~M-/~ for comment/uncomment.

;; [[https://github.com/redguardtoo/evil-nerd-commenter][source]]

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; (use-package evil-org
  ;; :ensure t
  ;; :after evil org
  ;; :init
  ;; (add-hook 'org-mode-hook #'evil-org-mode)
  ;; :config
  ;; (require 'evil-org-agenda)
  ;; (evil-org-agenda-set-keys)

;; 
(use-package cdlatex
  :demand t
  :ensure t)

(use-package citeproc
  :ensure t)

;; org mode
(use-package org
  :mode (("\\.org$" . org-mode))
  :straight (:repo "https://git.savannah.gnu.org/git/emacs/org-mode.git")
  :after citeproc cdlatex jupyter
  :demand t
  :ensure t
  :init
  ;; org-cdlatex mode https://orgmode.org/manual/CDLaTeX-mode.html
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  ;; spellchecking
  (add-hook 'org-mode-hook 'flyspell-mode)
  ;; capture
  (define-key global-map "\C-cc" 'org-capture)
  (add-hook 'org-capture-mode-hook #'org-align-all-tags)
  ;; agenda
  (define-key global-map "\C-ca" 'org-agenda)
  (setq dropsync-dir-parent (if in-termux-p "~/storage/shared/" "~/Dropbox/"))
  (setq dropsync-dir (concat dropsync-dir-parent "DropsyncFiles/"))
  (setq org-dir (concat dropsync-dir "org/"))
  (setq todo-file (concat org-dir "tasks.org"))
  (setq dtu-notes-file (concat org-dir "dtu.org"))
  (setq draft-file (concat org-dir "draft.org"))
  (setq org-agenda-window-setup 'only-window)  ;; agenda is full screen
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
  :config
  (unless in-termux-p
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)
       (jupyter . t)))
    ;; Don't prompt before running code in org
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  )
  ;; todo
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
  ;; export syntax highlighting
  (setq org-html-htmlize-output-type 'css)
  ;; ;; https://emacs.stackexchange.com/questions/3374/set-the-background-of-org-exported-code-blocks-according-to-theme
  ;; (defun my/org-inline-css-hook (exporter)
  ;;   "Insert custom inline css to automatically set the
  ;; background of code to whatever theme I'm using's background"
  ;;   (when (eq exporter 'html)
  ;;     (let* ((my-pre-bg (face-background 'default))
  ;;           (my-pre-fg (face-foreground 'default)))
  ;;       (setq
  ;;       org-html-head-extra
  ;;       (concat
  ;;         org-html-head-extra
  ;;         (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
  ;;                 my-pre-bg my-pre-fg))))))

  ;; (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

  ;; execute src blocks with C-<return>
  (define-key org-mode-map (kbd "<C-return>") 'org-babel-execute-src-block-maybe)
  :custom

  (org-image-actual-width 500)
  ;; babel
  (org-confirm-babel-evaluate nil)
  (ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  (org-image-actual-width nil) ;; images
  (org-src-fontify-natively t) ;; Syntax highlight in #+BEGIN_SRC blocks
  (org-edit-src-content-indentation 0)
  (org-indent_mode nil)
  (org-adapt-indentation nil)
  (org-cite-csl-styles-dir "~/Zotero/styles")
  (org-cite-csl-locales-dir "~/.emacs.d/straight/repos/org/etc/csl")
  (org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; capture
  (org-capture-templates
   '(("w" "Work todo" entry (file todo-file) "* TODO %i%? :work:\n%T")
     ("l" "Life todo" entry (file todo-file) "* TODO %i%? :life:\n%T")
     ("e" "Email" entry (file todo-file) "* TODO %a")
     ("t" "Misc todo" entry (file todo-file) "* TODO %i%?\n%T")
     ("d" "Draft" entry (file draft-file) "* %i%?\n%T")))
  )

(use-package ob-async
  :ensure t
  :unless in-termux-p)


;; org roam (for taking notes, not on termux for now)
(use-package org-roam
  :after org consult
  :unless in-termux-p
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  (defun tg/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep "rg --null --multiline --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
        (consult-ripgrep org-roam-directory)))
  :custom
  (org-roam-directory "~/Dropbox/DropsyncFiles/org/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry  "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "Default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags:")
      :unnarrowed t)
     ("m" "Meeting" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: %<%Y%m%d%H%M%S>-${title}\n#+filetags: :meeting:")
      :unnarrowed t)
     ("w" "Work" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :work:")
      :unnarrowed t)
     ("c" "Cat log" entry "** %T\n%?"
      :if-new (node "Cat log")
      :empty-lines-before 1
      :unnarrowed t)
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n n" . org-roam-capture)
         ("C-c n s" . tg/org-roam-rg-search)
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
  :after orderless
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles basic-remote partial-completion))))
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

(use-package consult-yasnippet
  :after consult
  :ensure t)

(use-package consult-lsp
 ;; provides these commands:

 ;; consult-lsp-diagnostics
 ;;     Select diagnostics from current workspace. Pass prefix argument to search all workspaces

 ;; consult-lsp-symbols
 ;;     Select symbols from current workspace. Pass prefix argument to search all workspaces.

 ;; consult-lsp-file-symbols
 ;;     Interactively select a symbol from the current file, in a manner similar to consult-line. 
  :ensure t)

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

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-count 15)
  (corfu-min-width 20)
  (corfu-max-width 100)
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match 0.5)        ;; Automatically quit if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-echo-documentation 0.25) ;; documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode)

  :config
  (with-eval-after-load "eldoc" (eldoc-add-command-completions "corfu-"))
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)
  :bind (:map corfu-map
         ("C-<return>" . corfu-insert)
         ("C-J" . corfu-next)
         ("C-K" . corfu-previous)
         ("C-<return>" . corfu-insert)))

;; corfu-related external config
(setq tab-always-indent 'complete)
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; bibtex actions
(defvar my/bibs '("/Users/tedgro/Reading/bibliography.bib"))

;; (use-package bibtex-completion
;;   :demand t
;;   :ensure t
;;   :config
;;   (setq bibtex-completion-additional-search-fields '(doi url)
;;         bibtex-completion-pdf-extension ".pdf"
;;         bibtex-completion-bibliography '("/Users/tedgro/Reading/bibliography.bib")
;;         bibtex-completion-pdf-field "file"
;;         bibtex-completion-pdf-open-function
;;         (lambda (fpath) (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))))

(use-package parsebib
  :demand t
  :straight (:type git :host github :repo "joostkremers/parsebib")
  :ensure t)

;; (use-package citar
;;   :ensure t
;;   :demand t
;;   :straight (:type git :host github :repo "bdarcus/citar")
;;   :bind (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
;;   ;; (("C-c b" . org-cite-insert)
;;          ;; ("M-o" . org-open-at-point)
;;          ;; :map minibuffer-local-map
;;          ;; ("M-b" . bibtex-actions-insert-preset))
;;   :after (embark oc citeprocciteproc)
;;   :custom
;;   (defun citar--fields-in-formats ()
;;     "Find the fields to mentioned in the templates."
;;     (seq-mapcat #'citar--fields-for-format
;;                 (list (citar-get-template 'main)
;;                       (citar-get-template 'suffix)
;;                       (citar-get-template 'preview)
;;                       (citar-get-template 'note))))
;;   (citar-bibliography my/bibs)
;;   (citar-at-point-function 'embark-act)
;;   (citar-file-open-function 'citar-file-open-external)
;;   (org-cite-global-bibliography my/bibs)
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar))

(use-package citar
  :no-require
  :straight (:type git :host github :repo "bdarcus/citar")
  :custom
  (org-cite-global-bibliography '("/Users/tedgro/Reading/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-file-open-function 'citar-file-open-external)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))



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

;; customize project.el
;; copied from here: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
(use-package project
  :straight nil
  :custom
  (project-switch-commands '((project-dired "Root" "r")
                             (project-find-file "File" "f")
                             (magit-project-status "Git" "g")
                             (tg/project-search "Search" "s")
                             (tg/vterm-dwim "Terminal" "t"))))

(defun tg/project-name (&optional project)
  "Return the name for PROJECT.
If PROJECT is not specified, assume current project root."
  (when-let (root (or project (tg/project-root)))
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory root)))))

(defun tg/project-search ()
  "Run ripgrep against project root.
If ripgrep is not installed, use grep instead."
  (interactive)
  (let ((root (tg/project-root)))
    (if (not (executable-find "rg"))
        (consult-grep root)
      (message "Could not find executable 'rg', using 'grep' instead")
      (consult-ripgrep root))))

(defun tg/project-root ()
  "Return the current project root."
  (when-let (project (project-current))
    (project-root project)))

(defun tg/vterm-dwim (&optional argument)
  "Invoke `vterm' according to context and current location."
  (interactive "P")
  (unless (require 'vterm nil :noerror)
    (error "Package 'vterm' not found"))
  (let ((project (tg/project-root)))
    (if (or argument (not project))
        (vterm)
      (let* ((buffer (format "*vterm: %s*" (tg/project-name project)))
             (replace (string= buffer (buffer-name)))
             (buffer (if replace (generate-new-buffer-name buffer) buffer)))
        (if (buffer-live-p (get-buffer buffer))
            (pop-to-buffer buffer)
          (let ((default-directory project))
            (vterm buffer)))))))

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
  :commands lsp lsp-deferred
  :after orderless
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-eldoc-enable-hover nil)
  (lsp-trigger-auto-activate nil)
  (lsp-file-watch-threshold 3000)
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  :init
  (defun tg/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold 100000000) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1mb https://emacs-lsp.github.io/lsp-mode/page/performance/
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'"))
  :hook
  ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  (python-mode . lsp-deferred)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . tg/lsp-mode-setup-completion))


;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; lsp-related packages

;; (use-package lsp-ui
  ;; :after lsp-mode
  ;; :commands lsp-ui-mode)

;; (use-package treemacs)

;; (use-package lsp-treemacs :after (lsp-mode treemacs) :commands lsp-treemacs-errors-list)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :custom
  (lsp-pyright-venv-path "/Users/tedgro/.venvs")
  (lsp-pyright-venv-directory "/Users/tedgro/.venvs")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-multi-root nil)
  (lsp-pyright-stub-path "/Users/tedgro/Code/stubs/"))


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
   "b" '(consult-buffer :which-key "consult-buffer")
   "cd" '(consult-lsp-diagnostics :which-key "consult-lsp-diagnostics")
   "cs" '(consult-lsp-symbols :which-key "consult-lsp-symbols")
   "cr" '(consult-ripgrep :which-key "consult-ripgrep")
   "cy" '(consult-yasnippet :which-key "consult-yasnippet")
   ;; "SPC" 'python-shell-send-statement
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
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env "bin/ipython")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "ipython")))))


;; ** formatting
(use-package py-isort
  :ensure t
  :init
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package blacken
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80)
  ;; (setq-default blacken-allow-py36 t)
  )

;; ** python-mode

(use-package python-mode
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  ;; (python-mode . company-mode)
  (python-mode . blacken-mode)
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
(use-package mu4e-alert
  :after mu4e
  :unless in-termux-p
  :init
  (setq mu4e-alert-interesting-mail-query
        "flag:unread AND (maildir:/gmail/Inbox OR maildir:/dtu/Inbox)")
  :config
  (mu4e-alert-disable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes
(use-package doom-themes
  :after org
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
  :after eshell doom-themes mu4e mu4e-alert     ;; load after eshell, doom theme and mu4e
  :custom-face
  ;; (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))
  :custom
  ;; (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
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
  (vterm-install t)
  (vterm-buffer-name-string "vterm %s"))

;; Stan
(use-package stan-mode
  ;; Uncomment if directly loading from your development repo
  ;; :load-path "your-path/stan-mode/stan-mode"
  :mode ("\\.stan\\'" . stan-mode)
  :ensure t
  ;; :hook (stan-mode . stan-mode-setup)
  ;;
  :custom
  ;; The officially recommended offset is 2.
  (stan-indentation-offset 2))

(use-package flycheck-stan
  :ensure t
  :after stan-mode
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable
        "/Users/tedgro/.cmdstan/cmdstan-2.28.1/bin/stanc"))

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
  :ensure t
  :unless in-termux-p)

;; youtube links (Prefix is 'yt')
;; (defvar yt-iframe-format
;;   ;; You may want to change your width and height.
;;   (concat "<iframe width=\"440\""
;;           " height=\"335\""
;;           " src=\"https://www.youtube.com/embed/%s\""
;;           " frameborder=\"0\""
;;           " allowfullscreen>%s</iframe>"))
;; (org-add-link-type
;;  "yt"
;;  (lambda (handle)
;;    (browse-url
;;     (concat "https://www.youtube.com/embed/"
;;             handle)))
;;  (lambda (path desc backend)
;;    (cl-case backend
;;      (html (format yt-iframe-format
;;                    path (or desc "")))
;;      (latex (format "\href{%s}{%s}"
;;                     path (or desc "video"))))))

;; (unless in-termux-p (require 'ox-bibtex))
;; (setq org-bibtex-file "/Users/tedgro/Dropbox/Reading/bibliography.bib")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRAMP
(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package olivetti
 :ensure t)
 ;; :custom
 ;; (olivetti-body-width (if in-termux-p 60 80))
 ;; :config
 ;; (use-package olivetti :ensure t)
 ;; (require 'olivetti)
 ;; (define-global-minor-mode global-olivetti-mode olivetti-mode
   ;; (lambda ()
     ;; (unless (memq major-mode '(minibuffer-mode which-key-mode mu4e-headers-mode))
       ;; (olivetti-mode 1))))
;; (if in-termux-p (setq-default fringe-mode nil))
 ;; (global-olivetti-mode 1)
 ;; )

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
  :init
  (setq mu4e-mu-binary (executable-find "mu"))
  :config

  ;; message on right hand side:
  ;; see https://www.djcbsoftware.nl/code/mu/mu4e/Split-view.html
  (setq mu4e-split-view 'vertical)
  (setq mu4e-headers-visible-columns 90)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

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
        '((:name  "Note to self"
                  :query "from:groves.teddy@gmail.com AND maildir:/gmail/Inbox"
                  :key ?n)
          (:name  "OpenTECR"
                  :query "list:opentecr.googlegroups.com AND NOT maildir:\"/gmail/[Google mail]/All Mail\""
                  :key ?o)
          (:name  "Github"
                  :query 
                  "from:notifications@github.com AND NOT maildir:\"/gmail/[Google mail]/All Mail\""
                  :key   ?g)
          (:name  "Discourse"
                  :query "from:mc_stan@discoursemail.com AND NOT maildir:\"/gmail/[Google mail]/All Mail\""
                  :key   ?d)
    ))

  ;; don't view html if plain text is available
  (with-eval-after-load "mm-decode"
       (add-to-list 'mm-discouraged-alternatives "text/html")
       ;; (add-to-list 'mm-discouraged-alternatives "text/richtext")
       )

  ;; date format in headers view
  (setq mu4e-headers-date-format "%Y-%m-%d")

  (setq mu4e-index-lazy-check t)
 

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
          :name "gmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "groves.teddy@gmail.com")
                    (user-full-name    . "Teddy Groves")
                    (mu4e-compose-signature . "Teddy via Gmail")
                    (mu4e-drafts-folder  . "/gmail/[Google mail]/Drafts")
                    (mu4e-sent-folder  . "/gmail/[Google mail]/Sent Mail")
                    (mu4e-refile-folder  . "/gmail/[Google mail]/All Mail")
                    (mu4e-trash-folder  . "/gmail/[Google mail]/Trash")))
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
    '((:maildir "/gmail/Inbox"    :key ?i)
      (:maildir "/gmail/[Google mail]/Sent Mail" :key ?s)
      (:maildir "/gmail/[Google mail]/Trash"     :key ?t)
      (:maildir "/gmail/[Google mail]/Drafts"    :key ?d)
      (:maildir "/gmail/[Google mail]/All Mail"  :key ?a)
      (:maildir "/gmail/newsletter"  :key ?n)
      (:maildir "/gmail/paper"  :key ?p)
      (:maildir "/dtu/Inbox"  :key ?I)
      (:maildir "/dtu/Drafts"    :key ?D))))

;;;;;;;;;;;;;;;;;;; accounting
(use-package hledger-mode
  :ensure t
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :commands hledger-enable-reporting
  :bind (("C-c j" . hledger-run-command))
  :custom (hledger-jfile "~/finance/2021.journal"))

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

;; Default fill column 80
(setq-default fill-column 80)

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
(setq-default mac-left-option-modifier 'meta)
(setq-default mac-right-option-modifier 'meta)
(setq-default mac-command-modifier 'super)
(global-set-key (kbd "s-k") 'kill-this-buffer)

;; font
;; (mac-auto-operator-composition-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(menu-bar-mode nil)
 '(org-agenda-files
   '("/Users/tedgro/Dropbox/DropsyncFiles/org/draft.org" "/Users/tedgro/Dropbox/DropsyncFiles/org/dtu.org" "/Users/tedgro/Dropbox/DropsyncFiles/org/groceries.org" "/Users/tedgro/Dropbox/DropsyncFiles/org/holiday.org" "/Users/tedgro/Dropbox/DropsyncFiles/org/tasks.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
