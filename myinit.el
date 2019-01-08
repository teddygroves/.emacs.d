(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq-default fill-column 80)

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

;; zenburn
 (use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package ob-ipython
 :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ipython . t)))
;don't prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)
;; from https://emacs.stackexchange.com/questions/30520/org-mode-c-c-c-c-to-display-inline-image
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-src-fontify-natively t)
(setq org-indent_mode nil)
(setq org-adapt-indentation nil)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(let* ((variable-tuple (cond ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
(custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

(setq org-todo-keywords '((sequence "TODO(t)" "BACKLOG(b)"
                                    "READY(r)" "IN PROGRESS(p)" "|" "DONE(d)")))

(define-key global-map "\C-cc" 'org-capture)

(setq org-agenda-files '("~/Dropbox/Writing/notes/inbox.org"
                         "~/Dropbox/Writing/notes/gtd.org"
                         "~/Dropbox/Writing/notes/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/Writing/notes/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/Writing/notes/inbox.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/Dropbox/Writing/notes/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/Writing/notes/tickler.org" :maxlevel . 2)
			   ("~/Dropbox/Writing/notes/topics.org" :maxlevel . 2)))

(defvar org-export-output-directory-prefix
 "export_"
 "prefix of directory used for org-mode export")

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
      (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
      (when (not (file-directory-p pub-dir))
       (make-directory pub-dir))))

(require 'ox-md nil t)

(defun my/org-md-paragraph-unfill (&rest args)
  "Unfill CONTENTS, the `cadr' in ARGS."
  (let* ((actual-args (car args))
         (org-el (nth 0 actual-args))
         (contents (nth 1 actual-args))
         (info (nth 2 actual-args)))
    ;; Unfill contents
    (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n"))
    (list org-el contents info)))
    (advice-add 'org-md-paragraph :filter-args #'my/org-md-paragraph-unfill)

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
   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
   ))

 (use-package ivy-hydra
  :ensure t)

(use-package auto-complete
 :ensure t
 :init
 (progn
   (ac-config-default)
   (global-auto-complete-mode t)
   ))

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

(evil-set-initial-state 'term-mode 'emacs)

(use-package stan-mode
  :ensure t)

(use-package stan-snippets
  :ensure t)

(use-package latex-preview-pane
  :ensure t)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

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
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (setq auto-revert-interval 0.5))
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

(use-package elpy
  :ensure t
  :config
    (progn
      ;; Use Flycheck instead of Flymake
      (when (require 'flycheck nil t)
        (remove-hook 'elpy-modules 'elpy-module-flymake)
        (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;;        (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
        (add-hook 'elpy-mode-hook 'flycheck-mode))
      (elpy-enable)
      (setq elpy-rpc-backend "jedi"))
      ;; use python 3
      ;;(setq elpy-rpc-python-command "python3")
      ;; see https://necromuralist.github.io/posts/org-babel-ipython-and-elpy-conflict/
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")
  )

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
  ;; Set the neo-window-width to the current width of the
  ;; neotree window, to trick neotree into resetting the
  ;; width back to the actual window width.
  ;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
    (eval-after-load "neotree"
     '(add-to-list 'window-size-change-functions
                   (lambda (frame)
                     (let ((neo-window (neo-global--get-window)))
                       (unless (null neo-window)
                         (setq neo-window-width (window-width neo-window)))))))

(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))

(require 'epa-file)
(setenv "GPG_AGENT_INFO" nil)
(epa-file-enable)

(use-package ace-window
  :ensure t)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; make sure passwords are in load path
(add-to-list 'load-path "~/.emacs.d/secrets/")

;; connections 
(setq sql-connection-alist
      '((datalake (sql-product 'mysql)
                   (sql-server "datalake.footballradar.net")
                   (sql-user "teddy.groves")
                   (sql-database "datalake"))
	(pmi_test (sql-product 'mysql)
                   (sql-server "127.0.0.1")
                   (sql-user "root")
                   (sql-database "pmi_test"))
        (playermodel (sql-product 'mysql)
                      (sql-server "mysql.prod.footballradar.net")
                      (sql-user "player_model")
                      (sql-database "playermodel"))))

(add-hook 'sql-interactive-mode-hook
        (lambda ()
          (toggle-truncate-lines t)))

(defun my-sql-connect (product connection)
  ;; load the password
  (require 'my-password "my-password.el.gpg")

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection my-sql-password)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))

(defun datalake ()
  (interactive)
  (my-sql-connect 'mysql 'datalake))

(defun playermodel ()
  (interactive)
  (my-sql-connect 'mysql 'playermodel))

(defun pmi_test ()
  (interactive)
  (my-sql-connect 'mysql 'pmi_test))

(setq-default indent-tabs-mode nil)

(use-package yasnippet
  :ensure t
  :init
 (yas-global-mode 1)
)

(auto-image-file-mode 1)
(global-auto-revert-mode 1)
