(use-package denote
  :ensure t
  :init
  (setq denote-directory (expand-file-name "~/Writing/notes/"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-allow-multi-word-keywords t)
  (setq denote-date-format nil) ; read doc string
  (setq denote-link-fontify-backlinks t)
  (defun org-project-completing-read ()
    "Select projects from the notes directory, using titles as prompts."
    (let* ((project-files (directory-files denote-directory t "_project"))
           (project-titles (mapcar 'denote--retrieve-value-title  project-files))
           (projects (-zip project-titles project-files)))
      (cdr (assoc (completing-read "Project: " projects) projects))))

    (with-eval-after-load 'org-agenda
      (setq org-agenda-files
            (append (directory-files denote-directory t "_project")
                    (directory-files denote-directory nil "inbox__"))))
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  :bind
  ("C-c n n" . denote)
  ("C-c n N" . denote-type)
  ("C-c n d" . denote-date)
  ("C-c n t" . denote-template)
  ("C-c n i" . denote-link) ; "insert" mnemonic
  ("C-c n I" . denote-link-add-links)
  ("C-c n l" . denote-link-find-file) ; "list" links
  ("C-c n b" . denote-link-backlinks))

  
(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  ;; :commands (consult-notes
             ;; consult-notes-search-in-all-notes
             ;; consult-notes-org-roam-find-node
             ;; consult-notes-org-roam-find-node-relation
             ;; )
  :bind
  ("C-c n f" . consult-notes)
  ("C-c n g" . consult-notes-search-in-all-notes)
  :config
  (setq consult-notes-sources `(("Notes"  ?n ,denote-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Example denote config ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember to check the doc strings of those variables.
;; (setq denote-directory (expand-file-name "~/Documents/notes/"))
;; (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
;; (setq denote-infer-keywords t)
;; (setq denote-sort-keywords t)
;; (setq denote-file-type nil) ; Org is the default, set others here
;; (setq denote-prompts '(title keywords))

;; Read this manual for how to specify `denote-templates'.  We do not
;; include an example here to avoid potential confusion.


;; We allow multi-word keywords by default.  The author's personal
;; preference is for single-word keywords for a more rigid workflow.
;; (setq denote-allow-multi-word-keywords t)

;; (setq denote-date-format nil) ; read doc string

;; By default, we fontify backlinks in their bespoke buffer.
;; (setq denote-link-fontify-backlinks t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; We use different ways to specify a path for demo purposes.
;; (setq denote-dired-directories
;;       (list denote-directory
;;             (thread-last denote-directory (expand-file-name "attachments"))
;;             (expand-file-name "~/Documents/books")))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Here is a custom, user-level command from one of the examples we
;; showed in this manual.  We define it here and add it to a key binding
;; below.
;; (defun my-denote-journal ()
;;   "Create an entry tagged 'journal', while prompting for a title."
;;   (interactive)
;;   (denote
;;    (denote--title-prompt)
;;    '("journal")))

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
;; (let ((map global-map))
;;   (define-key map (kbd "C-c n j") #'my-denote-journal) ; our custom command
;;   (define-key map (kbd "C-c n n") #'denote)
;;   (define-key map (kbd "C-c n N") #'denote-type)
;;   (define-key map (kbd "C-c n d") #'denote-date)
;;   (define-key map (kbd "C-c n s") #'denote-subdirectory)
;;   (define-key map (kbd "C-c n t") #'denote-template)
;;   ;; If you intend to use Denote with a variety of file types, it is
;;   ;; easier to bind the link-related commands to the `global-map', as
;;   ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
;;   ;; `markdown-mode-map', and/or `text-mode-map'.
;;   (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
;;   (define-key map (kbd "C-c n I") #'denote-link-add-links)
;;   (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
;;   (define-key map (kbd "C-c n b") #'denote-link-backlinks)
;;   ;; Note that `denote-rename-file' can work from any context, not just
;;   ;; Dired bufffers.  That is why we bind it here to the `global-map'.
;;   (define-key map (kbd "C-c n r") #'denote-rename-file)
;;   (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; ;; Key bindings specifically for Dired.
;; (let ((map dired-mode-map))
;;   (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
;;   (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
;;   (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

;; (with-eval-after-load 'org-capture
;;   (setq denote-org-capture-specifiers "%l\n%i\n%?")
;;   (add-to-list 'org-capture-templates
;;                '("n" "New note (with denote.el)" plain
;;                  (file denote-last-path)
;;                  #'denote-org-capture
;;                  :no-save t
;;                  :immediate-finish nil
;;                  :kill-buffer t
;;                  :jump-to-captured t)))

(require 'denote)
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (append org-capture-templates
                '(("n" "New note (with Denote)" plain
                    (file denote-last-path)
                    #'denote-org-capture
                    :no-save t
                    :immediate-finish nil
                    :kill-buffer t
                    :jump-to-captured t)
                  ("t" "Todo in project" entry
                    (file+headline org-project-completing-read "Tasks")
                    "* TODO %i%?\n%T")))))
