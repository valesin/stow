;; Initialize package management
;; This is removed because it has a conflict with straight.el
;; (require 'package)
;; (setq package-archives
;;       '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
;;         ("MELPA Stable" . "http://stable.melpa.org/packages/")
;;         ("MELPA"        . "http://melpa.org/packages/")))
;; (setq package-archive-priorities
;;       '(("MELPA Stable" . 5)
;;         ("MELPA"        . 10)
;;         ("GNU ELPA"     . 1)))
;; (package-initialize)
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-and-compile
;;   (setq use-package-always-ensure t ;; not necessary cause i always put it
;;         use-package-expand-minimally t))

;; Setup straight.el
;; Required bootstrap
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

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Required for certain functions
(require 'cl-lib)

;; Enable transparent encryption and decryption
(require 'epa-file)
(epa-file-enable)
(setq auth-sources '("~/.authinfo.gpg"))  ; auto login for WebDAV

;; Avoid uncomfortable checks
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Set lexical binding for faster execution
(setq lexical-binding t)

;;     promise (WHO NEEDS IT?)

;; Backup settings
(defvar --backup-directory (concat user-emacs-directory "backups/"))
(unless (file-exists-p --backup-directory)
  (make-directory --backup-directory t))
(setq backup-directory-alist `((".*" . ,--backup-directory)))
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)
(setq kept-old-versions 4)
(setq kept-new-versions 8)

;; Auto-save settings
(defvar --autosave-directory (concat user-emacs-directory "autosaves/"))
(unless (file-exists-p --autosave-directory)
  (make-directory --autosave-directory t))
(setq auto-save-default t)
(setq auto-save-timeout 20)
(setq auto-save-interval 200)
(setq auto-save-file-name-transforms
      `((".*" ,--autosave-directory t)))

;; Enable global auto-revert mode
(global-auto-revert-mode t)

;; Enable visual line mode in text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Type 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Set default browser for opening URLs
(setq browse-url-generic-program "firefox")

;; Uncomment the following lines to enable desktop save mode
;; (desktop-save-mode 1)
;; (setq desktop-restore-eager 10)

;; Uncomment for i3 integration
;; (require 'i3)
;; (require 'i3-integration)
;; (i3-one-window-per-frame-mode-on)
;; (i3-advise-visible-frame-list-on)
