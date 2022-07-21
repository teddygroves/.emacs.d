(use-package evil
  :ensure t
  :after undo-tree
  :custom
  (evil-undo-system 'undo-tree)
  (evil-mode-line-format '(before . mode-line-buffer-identification))
  :init
  (setq evil-want-integration t)
  (setq evil-shift-width 2)
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; tab does org-cycle in normal mode
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "C-<return>") #'org-open-at-point-global)
  ;; elfeed score map
  (evil-define-key 'normal elfeed-search-mode-map (kbd "v") 'tg/elfeed-score-search)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'vterm-mode 'emacs)
  ;; (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'view-mode 'emacs)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  ;; :custom (evil-collection-company-use-tng nil)
  :config
  (with-eval-after-load 'help (evil-collection-help-setup))
  (with-eval-after-load 'info (evil-collection-info-setup))
  (with-eval-after-load 'custom (evil-collection-custom-setup))
  (with-eval-after-load 'dired (evil-collection-dired-setup))
  (with-eval-after-load 'python (evil-collection-python-setup))
  (with-eval-after-load 'replace
    (evil-collection-occur-setup)
    (evil-collection-define-key 'normal 'occur-mode-map "q" 'quit-window))
  (with-eval-after-load
      'pdf-tools (evil-collection-pdf-setup)
      (evil-collection-define-key 'normal 'pdf-view-mode-map "l" 'pdf-history-backward)
      (evil-collection-define-key 'normal 'pdf-view-mode-map "r" 'pdf-history-forward))
  (with-eval-after-load 'magit (evil-collection-magit-setup))
  (with-eval-after-load 'which-key (evil-collection-which-key-setup))

  ;; so that SPC works as the general-define-key leader in dired buffers
  (evil-collection-define-key 'normal 'dired-mode-map " " nil))

;; evil org mode, but just for the agenda
(use-package evil-org
  :after evil org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; ** evil-nerd-commentor

;; Use ~M-/~ for comment/uncomment.

;; [[https://github.com/redguardtoo/evil-nerd-commenter][source]]

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

