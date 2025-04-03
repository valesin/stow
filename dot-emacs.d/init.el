;; init.el -- Main Emacs configuration

;; Add our custom lisp directory to load-path and load all its subdirectories.
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (let ((default-directory "~/.emacs.d/lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Use-package is used to group settings.
(use-package emacs
  :bind
  (
   ( "C-c 0 j t" .  (lambda () (interactive) (find-file "~/Documents/Personal/todo.org.gpg")))
   ( "C-c 0 j s" .  (lambda () (interactive) (find-file "~/Documents/Personal/someday.org.gpg")))
   )
  :init
   ;; Define directories for backups and autosaves.
  (defvar my-backup-directory (concat user-emacs-directory "backups/"))
  (defvar my-autosave-directory (concat user-emacs-directory "autosaves/"))

  ;; Create directories if they don't exist.
  (unless (file-exists-p my-backup-directory)
    (make-directory my-backup-directory t))
  (unless (file-exists-p my-autosave-directory)
    (make-directory my-autosave-directory t))

  ;; Set up autosaves.
  (setq auto-save-file-name-transforms
        `((".*" ,my-autosave-directory t)))
  (setq auto-save-default t)
  (setq auto-save-timeout 20)
  (setq auto-save-interval 200)

  ;; Set up backups.
  (setq backup-directory-alist
        `((".*" . ,my-backup-directory)))
  (setq make-backup-files t)
  (setq backup-by-copying t)
  (setq version-control t)
  (setq delete-old-versions -1)
  (setq delete-by-moving-to-trash t)
  (setq kept-old-versions 4)
  (setq kept-new-versions 8)

    ;; --- Override behavior for encrypted files ---
  ;; By default, Emacs disables backups and autosave for encrypted files.
  ;; The following advice forces backups to be created even for encrypted files.
  (defadvice epa-file (around force-backup activate)
    "Force backup creation for files opened via epa (encrypted files).
WARNING: Backup files will be unencrypted!"
    (let ((backup-enable-predicate (lambda (_file) t)))
      ad-do-it))

  ;; And add a hook to re-enable auto-save in encrypted buffers.
  (defun my/enable-autosave-for-encrypted ()
    "Force auto-save mode in encrypted buffers.
WARNING: Autosave files will be stored unencrypted!"
    (when (and buffer-file-name
               (string-match "\\.gpg\\'" buffer-file-name))
      (auto-save-mode 1)))
  (add-hook 'epa-file-hook 'my/enable-autosave-for-encrypted)
  
  :diminish visual-line-mode

  :hook ((text-mode . visual-line-mode)
         (emacs-startup . emacs-startup-screen))

  :custom
  ;; Main settings.
  (user-full-name "Valerio Siniscalco")
  (user-mail-address "valerio@siniscalco.me")
  ;; UI adjustments: hide menu, tool, and scroll bars.
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (line-spacing 0.1)
  (inhibit-startup-message t)
  (visible-bell nil)
  (ring-bell-function nil)
  (resize-mini-windows t)
  ;; Other custom settings (set here if you want to use custom-set-variables)
  (custom-file null-device)
  (browse-url-generic-program "firefox")
  (lexical-binding t)
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (save-interprogram-paste-before-kill t)

  :custom-face
  (default ((t (:family "Noto Sans Mono" :height 160))))

  :config
  (column-number-mode)
  (delete-selection-mode)
  (ido-mode t)
  (global-auto-revert-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)

  (defun emacs-startup-screen ()
    "Display the weekly org-agenda and all todos."
    (org-agenda nil "a"))
  )

(use-package cl-lib)

(use-package epa-file
  :custom
  (auth-sources '("~/.authinfo.gpg"))  ; auto login for WebDAV
  :config
  (epa-file-enable))

(use-package diminish)

;; Library setup
(use-package citeproc
  :defer t)

;; Load additional configuration files.
;;(load "org_config.el")
(org-babel-load-file "~/guix-conf/dotfiles/.emacs.d/org_config.org")

(use-package gnuplot
  :defer t)




;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

(use-package elfeed
  :bind      ; Global keybinding for elfeed
  ("C-x w" . elfeed))   ; Quick access to RSS reader

(use-package elfeed-org
  :after elfeed ; Load after elfeed is loaded
  :config     ; Configuration to run after loading
  (elfeed-org)   ; Initialize elfeed-org
  (setq rmh-elfeed-org-files (list "~/Documents/Personal/Reference/rssfeeds.org.gpg")))   ; Set RSS feeds org file

(use-package auctex)

(use-package ledger-mode
  :defer t)


