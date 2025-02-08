;; This folder and its subfolder, contain every function needed for the setup
;; The files are loaded individually
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

 ;; Global settings grouped using a use-package block
(use-package emacs
  ;; Bootstrap for straight.el
  :init
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
	(bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  :diminish visual-line-mode

  :hook (
	 (text-mode . visual-line-mode)
	 (emacs-startup . emacs-startup-screen)
	 )
  
  :custom
  ;; Main
  (user-full-name "Valerio Siniscalco")
  (user-mail-address "valerio@siniscalco.me")
  ;; UI adjustments: hide menu, tool, and scroll bars
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (line-spacing 0.1)
  (inhibit-startup-message t)
  (visible-bell nil)        
  (ring-bell-function nil)
  (resize-mini-windows t)

  ;; Behaviour
  (custom-file null-device)
  (browse-url-generic-program "firefox")
  (lexical-binding t)
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  ;; Autosaves
  (auto-save-default t)
  (auto-save-timeout 20)
  (auto-save-interval 200)
  (auto-save-file-name-transforms
   `((".*" ,--autosave-directory t)))

  ;; Backups
  (backup-directory-alist `((".*" . ,--backup-directory)))
  (make-backup-files t)
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (delete-by-moving-to-trash t)
  (kept-old-versions 4)
  (kept-new-versions 8)


  :custom-face
  (default ((t (:family "Noto Sans Mono" :height 160))))

    ;; Highlight selected region with a soft background.
  ;;(region ((t (:background "#a9bff9"))))

  ;; Customize active mode-line with a modern look.
  ;; (mode-line ((t (:foreground "#ffffff" 
  ;;                             :background "#4C566A"
  ;;                             :box (:line-width -1 :color "#4C566A")))))

    
  ;; Customize inactive mode-line with subdued colors.
  ;; (mode-line-inactive ((t (:foreground "#d8dee9"
  ;;                                      :background "#2E3440"
  ;;                                      :box (:line-width -1 :color "#2E3440")))))

  ;; ;; Set the fringe (gutter) a slightly darker background.
  ;; (fringe ((t (:background "#000000"))))

  ;;   ;; Highlight prompts in the minibuffer.
  ;; (minibuffer-prompt ((t (:foreground "#88C0D0" :weight bold)))))

  ;; (header-line ((t (:background "#3B4252" :foreground "#ECEFF4" :box (:line-width 1 :color "#3B4252")))))

  
  :config
  (column-number-mode)
  (delete-selection-mode)
  (ido-mode)
  (global-auto-revert-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Create backup directory if it doesn't exist
  (defvar --backup-directory (concat user-emacs-directory "backups/"))
  (unless (file-exists-p --backup-directory)
    (make-directory --backup-directory t))
  
  ;; Create autosave directory if it doesn't exist
  (defvar --autosave-directory (concat user-emacs-directory "autosaves/"))
  (unless (file-exists-p --autosave-directory)
    (make-directory --autosave-directory t))

  (defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "a"))
  )


(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-recipes-gnu-elpa-use-mirror t)
  :config
  (straight-use-package 'use-package)
  )

(use-package cl-lib
  :straight nil)

(use-package epa-file
  :straight nil
  :custom
  (auth-sources '("~/.authinfo.gpg"))  ; auto login for WebDAV
  :config
  (epa-file-enable))

(load "org_config.el")
(load "org_roam_config.el")
(load "programming_config.el")
;; ;;(load "ocaml_config.el")
;; ;;(load "go_config.el")
(load "elfeed_config.el")
(load "latex_config.el")
(load "bibliography_config.el")
(load "ledger_config.el")
 
