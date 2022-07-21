(defvar tg/bibliography
  (list (concat tg/icloud-drive "Documents/Reading/bibliography.bib"))
  "My bibliography file")

(use-package citar
  :after org
  :demand t
  :straight (:type git :host github :repo "bdarcus/citar")
  :custom
  (citar-bibliography tg/bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-file-open-function 'find-file)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  ("C-c b" . #'citar-insert-citation)
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(with-eval-after-load 'org
  (setq org-cite-global-bibliography tg/bibliography
        org-cite-csl-styles-dir (concat tg/icloud-drive "Documents/Reading/Zotero/styles")
        org-cite-csl-locales-dir "~/.emacs.d/straight/repos/org/etc/csl"))
