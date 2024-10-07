(require 'package)
(package-initialize)
;;just to solve spinner issue
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
'(("GNU ELPA" . "http://elpa.gnu.org/packages/")
("MELPA Stable" . "http://stable.melpa.org/packages/")
("MELPA" . "http://melpa.org/packages/"))
package-archive-priorities
'(("MELPA Stable" . 10)
("MELPA" . 5)
("GNU ELPA" . 0)))
;; Comment out if you've already loaded this package...
(require 'cl-lib)

(global-auto-revert-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; fontify code in code blocks
(setq org-src-fontify-natively t)

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

;; this folder contains every package used
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'htmlize)

;; set init file in config position
;;(setq user-init-file "~/.config/emacs/init.el")

;; GO_____________________________________
;; set 4 tabs
;;(add-mode-hook 'go-mode-hook (lambda ()
;;			       (setq tab-width 4)))
;; enable snippets
(require 'yasnippet)
(add-hook 'go-mode-hook #'yas-minor-mode)
(add-to-list 'yas-snippet-dirs "/home/vko/Documents/Uni/Alg/")

;; gofmt at save
(add-hook 'before-save-hook 'gofmt-before-save)

;;Global settings
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-14"))
(setq line-spacing 0.1)          ; Set line spacing
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell t)            ; Flash when the bell rings
;(load-theme 'modus-vivendi t)   ; Load the Modus Vivendi dark theme
(set-face-attribute 'default nil :height 140); Change font size
(menu-bar-mode -1) ; Hide menu bar
(tool-bar-mode -1) ; Hide tool bar
(scroll-bar-mode -1) ; Hide scroll bar

;; By default, Emacs saves backup files in the current directory. These are the files ending in ~ that are cluttering up your directory lists.
;; The following code stashes them all in ~/.config/emacs/backups, where I can find them with C-x C-f (find-file) if I really need to. 
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(with-eval-after-load 'tramp
(add-to-list 'tramp-backup-directory-alist
             (cons tramp-file-name-regexp nil)))

; Type y or p instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; i3 integration
(require 'i3)
(require 'i3-integration)
(i3-one-window-per-frame-mode-on)
(i3-advise-visible-frame-list-on)

(require 'company)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
(defun lsp-go-install-save-hooks()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Install the package from https://github.com/emacsmirror/spray?tab=readme-ov-file to speedread
;(require 'spray)

;(require 'ledger-mode)


;________________________________________________________________________________________________________________
;;ORG related settings
(require 'org)
;; Global commands for org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;set stages
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;;set files to fill agenda
(setq org-agenda-files '("~/Documents/gtd.org"))
;; hide emphasis markers (*..*, /../)
(setq org-hide-emphasis-markers t)
;; sostitute bullet circle to the hyphen for list items
;;(font-lock-add-keywords 'org-mode
;;                          '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; setup indentation space
(setq org-indent-indentation-per-level 4)
;; activate it on startup
(setq org-startup-indented t)
;; Follow the links
(setq org-return-follows-link  t)
;;Non-nil means font-lock should hide the emphasis marker characters.
(setq org-hide-emphasis-markers t)
;;C-a and C-e bring to beginning of item first (after stars and TODO), then beginning of line
(setq org-special-ctrl-a/e t)
;; Like before, check specifics and update
(setq  org-special-ctrl-k t)
;;   log starting time for TODO lists
(setq org-log-done t)
;;   M-Ret creates and goes to a new blank line under the current
(setq org-M-RET-may-split-line nil)
;; Cool bullets
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Setup capture mode to quickly register tasks

;; (setq org-capture-templates
;;            '(("t" "Todo" entry (file+headline "~/Documents/gtd.org" "Tasks")
;;               "* TODO %?\n  %i\n  %a")
;;              ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;               "* %?\nEntered on %U\n  %i\n  %a")))
;; The code above saves also the place from where the capture was started, through %a. Useful for emails. I temporarily remove it.
(setq org-capture-templates
           '(("t" "Todo" entry (file+headline "~/Documents/Planner/gtd.org" "Tasks")
              "* TODO %?")
             ("j" "Journal" entry (file+datetree "~/Documents/Planner/journal.org")
              "* %?\nEntered on %U")))

(setq org-refile-use-outline-path t)
;; Refile until 5 level
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

;; ORG AGENDA
;; (setq org-agenda-start-on-weekday 1)
;; (setq org-agenda-skip-scheduled-if-done t)
;; (setq org-agenda-skip-deadline-if-done t)
;; (setq org-agenda-todo-ignore-scheduled t)

;; ORG BABEL
 (require 'ob-go);; no because it adds the package and i can't tangle
;; load languages
     (org-babel-do-load-languages
      'org-babel-load-languages
      '(
	(emacs-lisp . nil)
        (ocaml . t)
	(go . t)
	)
      )

(require 'ob-ocaml)
;; Check this stuff in the future
;;   ; Log stuff into the LOGBOOK drawer by default
;;   (org-log-into-drawer t)
;;   ;Enable Speed Keys, which allows quick single-key commands when the cursor is placed on a heading. Usually the cursor needs to be at the beginning of a headline line, but defining it with this function makes them active on any of the asterisks at the beginning of the line (useful with the font highlighting I use, as all but the last asterisk are sometimes not visible).
;;   (org-use-speed-commands
;;    (lambda ()
;;    (and (looking-at org-outline-regexp)
;;         (looking-back "^\**"))))  




;; the following should be a nice beautifying but i don't find it useful
;; (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
;;    '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160))))
;;    '(org-block ((t (:inherit fixed-pitch))))
;;    '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-document-info ((t (:foreground "dark orange"))))
;;    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;    '(org-link ((t (:foreground "royal blue" :underline t))))
;;    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-property-value ((t (:inherit fixed-pitch))) t)
;;    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;    '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (add-hook 'org-mode-hook 'visual-line-mode)
;;
;; This one in particulare makes incremental sized level with the same color. I like it, but i don't like the fonts
;; (let* ((variable-tuple (cond ((x-list-fonts "ETBook") '(:font "ETBook"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; (add-hook 'tuareg-mode-hook
;;             (lambda ()
	      
;;               ))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(yasnippet org tuareg ledger-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
