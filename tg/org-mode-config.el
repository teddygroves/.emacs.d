(defvar tg/icloud-drive
  "/Users/tedgro/Library/Mobile Documents/com~apple~CloudDocs/"
  "Location of my icloud drive")

(defvar tg/org-dir (concat tg/icloud-drive "Documents/org/") "Where my org documents live")

(use-package org
  :mode (("\\.org$" . org-mode))
  :straight (:repo "https://git.savannah.gnu.org/git/emacs/org-mode.git")
  :demand t
  :bind (("C-c c" . org-capture)
         ("C-c a" . 'org-agenda))
         ;; :map org-mode-map
         ;; ("C-j" . forward-page)
         ;; ("C-k" . backward-page))
  :ensure t
  :init
  ;; org-cdlatex mode https://orgmode.org/manual/CDLaTeX-mode.html
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)

  ;; capture hook
  (add-hook 'org-capture-mode-hook #'org-align-all-tags)

  :config
  ;; nicer timestamps from https://endlessparentheses.com/better-time-stamps-in-org-export.html
  (with-eval-after-load 'ox (add-to-list 'org-export-filter-timestamp-functions
               #'tg/filter-timestamp))
  (defun tg/filter-timestamp (timestamp backend _comm)
    "Remove <> around time-stamps."

    (cond
     ((org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
     ((org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats
        '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))


  ;; org habit
  (add-to-list 'org-modules 'org-habit)

  ;; execute src blocks with C-<return>
  (define-key org-mode-map (kbd "<C-return>") 'org-babel-execute-src-block-maybe)

  :custom
  ;; export syntax highlighting
  (org-html-htmlize-output-type 'css)

  ;; init file for async export emacs process
  (org-export-in-background t)
  (org-export-async-init-file "~/.emacs.d/tg/org-async-export-init.el")
  
  ;; todo
  (org-todo-keywords '((sequence "TODO" "|" "DONE")))
  (todo-file (concat tg/org-dir "tasks.org"))

  ;; misc
  (org-return-follows-link t)
  (org-image-actual-width 500)

  ;; babel
  (org-confirm-babel-evaluate nil)
  (ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  (org-image-actual-width nil) ;; images
  (org-src-fontify-natively t) ;; Syntax highlight in #+BEGIN_SRC blocks
  (org-edit-src-content-indentation 0)
  (org-indent_mode nil)
  (org-adapt-indentation nil)
  (org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; habits
  (org-habit-show-all-today t)
  (org-habit-show-done-always-green t)
  (org-habit-completed-glyph ?✓)
  (org-habit-today-glyph ?|)

  ;; agenda
  (org-agenda-window-setup 'other-window)  ;; agenda is full screen
  (org-agenda-prefix-format " %i %?-20c% s")
  (org-agenda-hide-tags-regexp "work\\|project\\|Inbox")
  (org-agenda-custom-commands
   `(("w" "Work view"
      ((agenda "" ((org-agenda-span 5)
                   ;; (org-agenda-start-day "+-1d")
                   (org-agenda-start-on-weekday 1)
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))))
       (agenda "" ((org-agenda-time-grid nil)
                   (org-agenda-start-on-weekday nil)
                   ;; We don't want to replicate the previous section's
                   ;; three days, so we start counting from the day after.
                   (org-agenda-start-day "+5d")
                   (org-agenda-span 14)
                   (org-agenda-show-all-dates nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Upcoming deadlines (+5-14d)")))
       (agenda "" ((org-agenda-span 1)
                   (org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit")
                         (org-agenda-skip-entry-if 'regexp ":pause:")))
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   (org-agenda-format-date "")
                   (org-agenda-overriding-header "Habits")))
       (tags-todo "-pause-STYLE=\"habit\"" ((org-agenda-overriding-header "Active tasks")))
       (tags-todo "+pause-STYLE=\"habit\"" ((org-agenda-overriding-header "Paused tasks"))))
      ((org-agenda-tag-filter-preset '("+work"))))
     ("l" "Life view"
      ((agenda "" ((org-agenda-span 5)
                   (org-agenda-start-day "+-1d")
                   ;; (org-agenda-start-on-weekday nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))))
       (agenda "" ((org-agenda-time-grid nil)
                   ;; (org-agenda-start-on-weekday nil)
                   ;; We don't want to replicate the previous section's
                   ;; three days, so we start counting from the day after.
                   (org-agenda-start-day "+5d")
                   (org-agenda-span 14)
                   (org-agenda-show-all-dates nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-block-separator nil)
                   (org-agenda-entry-types '(:deadline))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Upcoming deadlines (+5-14d)")))
       (agenda "" ((org-agenda-span 1)
                    (org-agenda-skip-function
                    '(or (org-agenda-skip-entry-if 'notregexp ":STYLE:.*habit")
                         (org-agenda-skip-entry-if 'regexp ":pause:")))
                    (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                    (org-agenda-format-date "")
                    (org-agenda-overriding-header "Habits")))
       (tags-todo "-pause-STYLE=\"habit\"" ((org-agenda-overriding-header "Active tasks")))
       (tags-todo "+pause" ((org-agenda-overriding-header "Paused tasks"))))
      ((org-agenda-tag-filter-preset '("-work")))))))

(use-package ob-async
  :ensure t)

(defun tg/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
    (lambda ()
      (org-archive-subtree)
      (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
    "/DONE" 'file))


;; ox-ipynb
(use-package ox-ipynb
  :after org
  :ensure t
  :straight (:type git :host github :repo "jkitchin/ox-ipynb"))



;; jupyter
(use-package jupyter
  :unless tg/in-termux
  :ensure t
  :config (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))
  )

;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (jupyter . t)))

;; Don't prompt before running code in org
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
