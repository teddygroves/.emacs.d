;;;;;;;;;;;;;;;;;;;;;;;;; osm: open street mapper on emacs
(use-package osm
  :ensure t
  :straight (:type git :host github :repo "minad/osm")
  :after org
  :bind (("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump))
  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t)     ;; Display the copyright information
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

;;;;;;;;;;;;;;;;;;;;;;; logos: a package for nicely formatted narrowing and focusing
(use-package logos
  :after outline
  :ensure t
  :straight (:host github :repo "protesilaos/logos")
  :custom
  (logos--olivetti t)
  (logos-outlines-are-pages t)
  (logos-outline-regexp-alist
   `((emacs-lisp-mode . "^;;;+ ")
     (org-mode . "^\\*\\* +")
     (markdown-mode . "^\\#+ +")
     (t . ,(or outline-regexp logos--page-delimiter))))
  :init
  (defun logos--reveal-entry ()
    "Reveal Org or Outline entry."
    (cond
    ((and (eq major-mode 'org-mode)
      (org-at-heading-p))
     (org-show-subtree)
     (org-cycle-hide-drawers 'all)
     )
    ((or (eq major-mode 'outline-mode)
      (bound-and-true-p outline-minor-mode))
      (outline-show-subtree))))
  (add-hook 'logos-page-motion-hook #'logos--reveal-entry)
  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti nil)
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode)))
