;; PACKAGES
(require 'package)
(package-initialize)
(require 'cl-lib)

(setq package-archives
'(("GNU ELPA" . "http://elpa.gnu.org/packages/")
("MELPA Stable" . "http://stable.melpa.org/packages/")
("MELPA" . "http://melpa.org/packages/"))
package-archive-priorities
'(("MELPA Stable" . 10)
("MELPA" . 5)
("GNU ELPA" . 0)))

(defvar my-packages
  '(ledger-mode org lsp-mode lsp-ui go-mode company yasnippet)
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
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(with-eval-after-load 'tramp
(add-to-list 'tramp-backup-directory-alist
             (cons tramp-file-name-regexp nil)))


;; save as many files as possible
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; Open files in firefox
(setq browse-url-generic-program "firefox")

;; Keep open files open across sessions.

(desktop-save-mode 1)
(setq desktop-restore-eager 10)

