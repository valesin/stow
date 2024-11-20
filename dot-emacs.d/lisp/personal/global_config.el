;; PACKAGES
(require 'package)
(package-initialize)
(require 'cl-lib)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "http://stable.melpa.org/packages/")
        ("MELPA" . "http://melpa.org/packages/")))

(setq package-archive-priorities
      '(("MELPA Stable" . 5)
        ("MELPA" . 10)
        ("GNU ELPA" . 1)))

;; set lexical binding for faster execution
(setq lexical-binding t)

(defvar my-packages
  '(ledger-mode org lsp-mode lsp-ui go-mode company yasnippet auctex org-roam zotxt elfeed elfeed-org use-package consult promise org-anki)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
;; end PACKAGES

;; handle backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
;;(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))


(global-auto-revert-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)


;;Type y or p instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; i3 integration
;(require 'i3)
;(require 'i3-integration)
;(i3-one-window-per-frame-mode-on)
;(i3-advise-visible-frame-list-on)

;; By default, Emacs saves backup files in the current directory. These are the files ending in ~ that are cluttering up your directory lists.
;; The following code stashes them all in ~/.config/emacs/backups, where I can find them with C-x C-f (find-file) if I really need to. 
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(with-eval-after-load 'tramp
(add-to-list 'tramp-backup-directory-alist
             (cons tramp-file-name-regexp nil)))



;; Open files in firefox
(setq browse-url-generic-program "firefox")

;; Keep open files open across sessions.
;(desktop-save-mode 1)
;(setq desktop-restore-eager 10)

