(use-package mu4e
  :straight (:local-repo "/opt/homebrew/Cellar/mu/1.6.11/share/emacs/site-lisp/mu/mu4e/"
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
        ;;  (make-mu4e-context
        ;;   :name "dtu"
        ;;   :match-func
        ;;   (lambda (msg)
        ;;     (when msg
        ;;       (string-prefix-p "/dtu" (mu4e-message-field msg :maildir))))
        ;;     :vars '((user-mail-address . "tedgro@dtu.dk")
        ;;             (user-full-name    . "Teddy Groves")
        ;;             (mu4e-compose-signature . "Teddy Groves via DTU email")
        ;;             (mu4e-drafts-folder  . "/dtu/Drafts")
        ;;             (mu4e-sent-folder  . "/dtu/Sent Items")
        ;;             (mu4e-refile-folder  . "/dtu/Archive")
        ;;             (mu4e-trash-folder  . "/dtu/Deleted Items")))
        ;;  )
        )

  (setq mu4e-maildir-shortcuts
    '((:maildir "/gmail/Inbox"    :key ?i)
      (:maildir "/gmail/[Google mail]/Sent Mail" :key ?s)
      (:maildir "/gmail/[Google mail]/Trash"     :key ?t)
      (:maildir "/gmail/[Google mail]/Drafts"    :key ?d)
      (:maildir "/gmail/[Google mail]/All Mail"  :key ?a)
      (:maildir "/gmail/newsletter"  :key ?n)
      (:maildir "/gmail/paper"  :key ?p)
      ;; (:maildir "/dtu/Inbox"  :key ?I)
      ;; (:maildir "/dtu/Drafts"    :key ?D)
      ))))

;; (use-package mu4e-alert
;;   :after mu4e
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;         "flag:unread AND (maildir:/gmail/Inbox OR maildir:/dtu/Inbox)")
;;   :config
;;   (mu4e-alert-disable-notifications)
;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

(with-eval-after-load 'evil-collection (evil-collection-mu4e-setup))
