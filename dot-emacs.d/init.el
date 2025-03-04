;; init.el -- Main Emacs configuration

;; Add our custom lisp directory to load-path and load all its subdirectories.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Use-package is used to group settings.
(use-package emacs
  :bind
  (
   ( "C-c 0 j t" .  (lambda () (interactive) (find-file "~/Documents/Personal/todo.org.gpg")))
   ( "C-c 0 j s" .  (lambda () (interactive) (find-file "~/Documents/Personal/someday.org.gpg")))
   )
  :init
  ;; Bootstrap for straight.el
  (defvar bootstrap-version)
  (let* ((bootstrap-file
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

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-recipes-gnu-elpa-use-mirror t)
  :config
  (straight-use-package 'use-package))

(use-package cl-lib
  :straight nil)

(use-package epa-file
  :straight nil
  :custom
  (auth-sources '("~/.authinfo.gpg"))  ; auto login for WebDAV
  :config
  (epa-file-enable))

(use-package diminish)

;; Load additional configuration files.
(load "org_config.el")
(load "org_roam_config.el")
(load "programming_config.el")
;; (load "ocaml_config.el")
;; (load "go_config.el")
(load "elfeed_config.el")
(load "latex_config.el")
(load "bibliography_config.el")
(load "ledger_config.el")
