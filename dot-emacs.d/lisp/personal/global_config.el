;; Initialize package management
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "http://stable.melpa.org/packages/")
        ("MELPA"        . "http://melpa.org/packages/")))
(setq package-archive-priorities
      '(("MELPA Stable" . 5)
        ("MELPA"        . 10)
        ("GNU ELPA"     . 1)))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t ;; not necessary cause i put it
        use-package-expand-minimally t))

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

;; ;; Ensure packages are installed at launch
;; (defvar my-packages
;;     lsp-mode
;;     lsp-ui
;;     go-mode
;;     company
;;     yasnippet
;; 
;;     
;;    
;;     
;;     promise

;; (defun my-packages-installed-p ()
;;   "Check if all packages in `my-packages` are installed."
;;   (cl-loop for p in my-packages
;;            when (not (package-installed-p p)) do (cl-return nil)
;;            finally (cl-return t)))

;; (unless (my-packages-installed-p)
;;   ;; Check for new package versions
;;   (package-refresh-contents)
;;   ;; Install missing packages
;;   (dolist (p my-packages)
;;     (unless (package-installed-p p)
;;       (package-install p))))

;; Handle backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p --backup-directory)
  (make-directory --backup-directory t))

(setq backup-directory-alist `(("." . ,--backup-directory))
      make-backup-files t                ; backup files the first time they are saved
      backup-by-copying t                ; don't clobber symlinks
      version-control t                  ; use version numbers for backups
      delete-old-versions t              ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6                ; oldest versions to keep
      kept-new-versions 9                ; newest versions to keep
      auto-save-default t                ; auto-save every buffer that visits a file
      auto-save-timeout 20               ; number of seconds idle time before auto-save
      auto-save-interval 200             ; number of keystrokes between auto-saves
      )

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
