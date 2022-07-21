(use-package auctex
  :straight (:type git
             :host nil
             :repo "https://git.savannah.gnu.org/git/auctex.git"
             :pre-build ((shell-command "./autogen.sh && ./configure --without-texmf-dir --with-lispdir=. && make")))
  :mode
  ; https://www.mail-archive.com/auctex@gnu.org/msg07608.html
  ; https://www.gnu.org/software/emacs/manual/html_node/reftex/Installation.html
  ("\\.tex\\'" . latex-mode) ; Must first activate the inferior Emacs latex mode
  :hook
          (LaTeX-mode . TeX-PDF-mode)
          (LaTeX-mode . company-mode)
          (LaTeX-mode . flyspell-mode)
          (LaTeX-mode . flycheck-mode)
          (LaTeX-mode . LaTeX-math-mode)
          (LaTeX-mode . turn-on-reftex)
          (LaTeX-mode . turn-on-cdlatex)
  :init
  ;; (load "auctex.el" nil t t)
  ;; (load "preview-latex.el" nil t t)
  (require 'reftex) 
  (setq-default TeX-master 'dwim)
  (setq TeX-data-directory (straight--repos-dir "auctex")
        TeX-lisp-directory TeX-data-directory                   
        ; Or custom-set-variables as follows.
        ; M-x describe-variable RET preview-TeX-style-dir RET
        ;`(preview-TeX-style-dir ,(concat ".:" (straight--repos-dir "auctex") "latex:"))
        preview-TeX-style-dir (concat ".:" (straight--repos-dir "auctex") "latex:")
        TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        TeX-auto-untabify t ; Automatically remove all tabs from a file before saving it. 
        ;Type of TeX engine to use.
        ;It should be one of the following symbols:
        ;* ‘default’
        ;* ‘luatex’
        ;* ‘omega’
        ;* ‘xetex’
        TeX-engine 'default
        TeX-auto-local ".auctex-auto" ; Directory containing automatically generated TeX information.
        TeX-style-local ".auctex-style" ; Directory containing hand generated TeX information.
      
        ;; ##### Enable synctex correlation. 
        ;; ##### From Okular just press `Shift + Left click' to go to the good line. 
        ;; ##### From Evince just press `Ctrl + Left click' to go to the good line.      
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
     
        ;; automatically insert braces after sub/superscript in math mode
        TeX-electric-sub-and-superscript t 
        ;; If non-nil, then query the user before saving each file with TeX-save-document.  
        TeX-save-query nil
        
        TeX-view-program-selection '((output-pdf "PDF Tools"))))

(use-package cdlatex
  :demand t
  :ensure t)
