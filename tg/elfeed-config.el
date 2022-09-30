(use-package elfeed
  :after evil
  :ensure t
  :init
  (setq elfeed-show-entry-switch #'display-buffer)
  (evil-define-key 'normal-mode elfeed-show-mode-map (kbd "q") #'delete-window)
  (global-set-key (kbd "C-x w") 'elfeed)
  (run-at-time nil (* 8 60 60) #'elfeed-update))

(use-package elfeed-org
  :after elfeed
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  :init
  (elfeed-org))

(use-package elfeed-score
  :after elfeed elfeed-org
  :ensure t
  :custom
  (setq elfeed-score-score-format '("%d " 4 :right))
  :config
  (progn
    (setq elfeed-score-score-file "~/.emacs.d/elfeed-score.el")
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)
    (setq elfeed-search-print-entry-function #'elfeed-score-print-entry)))

(use-package elfeed-tube
  :straight (:host github :repo "karthink/elfeed-tube")
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;; mpv: a package for connecting to the mpv video player
(use-package mpv
  :ensure t)

(use-package elfeed-tube-mpv
  :straight (:host github :repo "karthink/elfeed-tube")
  :custom
  (elfeed-tube-mpv-options '("--cache=yes" "--ytdl-format=`bestvideo[height<=?1080][vcodec!=vp9]+bestaudio/best`"))
  :bind (:map elfeed-show-mode-map
              ("C-c C-p" . mpv-pause)
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(defun tg/elfeed-score-search ()
  (interactive)
  (elfeed-score-score-search))

(with-eval-after-load 'evil-collection (evil-collection-elfeed-setup))

(defun blfeed-olivetti-other-window (buff)
  (interactive)
  (pop-to-buffer buff)
  (olivetti-mode)
  (elfeed-show-refresh)
  (pop-to-buffer "*elfeed-search*"))

(defun elfeed-olivetti (buff)
  (switch-to-buffer buff)
  (olivetti-mode)
  (elfeed-show-refresh))

(setq elfeed-show-entry-switch 'elfeed-olivetti)
