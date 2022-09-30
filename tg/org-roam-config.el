(use-package emacsql
  :straight (:type git :host github :repo "bram85/emacsql")
  :ensure t)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat tg/icloud-drive "Documents/org/org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry  "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("d" "Default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags:")
      :unnarrowed t)
     ("m" "Meeting" entry "* %T ${title}\n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n%?"
      :target (file "20220113161734-meetings.org")
      :unnarrowed t)
     ("w" "Work" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :work:")
      :unnarrowed t)
     ("c" "Cat log" entry "** %T\n%?"
      :target (file+olp "20211228121509-axel.org" ("Cat log"))
      :empty-lines-before 1
      :unnarrowed t)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n n" . org-roam-capture))
         ;; ("C-c n p" . tg/org-roam-find-project)
         ;; ("C-c n b" . tg/org-roam-capture-inbox)
         ;; ("C-c n s" . tg/org-roam-rg-search)
         ;; ("C-c n t" . tg/org-roam-capture-task)
         ;; :map org-mode-map
         ;; ("C-M-i" . completion-at-point)
         ;; :map org-roam-dailies-map
         ;; ("Y" . org-roam-dailies-capture-yesterday)
         ;; ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-db-autosync-mode)
  ;; Build the agenda list the first time for the session
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (add-to-list 'display-buffer-alist
              '("\\*org-roam\\*"
                (display-buffer-in-direction)
                (direction . right)
                (window-width . 0.33)
                (window-height . fit-window-to-buffer)))
  (defun tg/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  (defun tg/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
            (tg/org-roam-filter-by-tag tag-name)
            (org-roam-node-list))))
  (defun tg/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (append (tg/org-roam-list-notes-by-tag "project")
                                    (tg/org-roam-list-notes-by-tag "inbox"))))
  (defun tg/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (let ((consult-ripgrep "rg --null --multiline --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
        (consult-ripgrep org-roam-directory)))
  (defun tg/org-roam-capture-task ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'tg/org-roam-project-finalize-hook)
    ;; Capture the new task, creating the project file if necessary
    (org-roam-capture-
      :node (org-roam-node-read
            nil
            (lambda (node) (member "project" (org-roam-node-tags node))))
      :templates '(("p" "project" plain "** TODO %?"
                    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: project"
                                          ("Tasks"))))))
  (defun tg/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
  capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'tg/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))
  (defun tg/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'tg/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
    nil
    nil
    (tg/org-roam-filter-by-tag "project")
    :templates
    '(("p" "project" plain "* Tasks\n\n** TODO Add initial tasks\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: project")
        :unnarrowed t))))
  (defun tg/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                      :templates '(("i" "inbox" plain "* %?"
                                    :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))
  (tg/org-roam-refresh-agenda-list))
