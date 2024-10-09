;;ORG related settings
(require 'org)
(require 'htmlize)
(require 'cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
;; Global commands for org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;set stages
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;;set files to fill agenda
(setq org-agenda-files '("~/Documents/Planner/"))
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

;; fontify exports
(setq org-src-fontify-natively t)

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

;; use whole path to refile
;;(setq org-refile-use-outline-path t)
;; Refile until 5 level
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

;; ORG AGENDA
;; (setq org-agenda-start-on-weekday 1)
;; (setq org-agenda-skip-scheduled-if-done t)
;; (setq org-agenda-skip-deadline-if-done t)
;; (setq org-agenda-todo-ignore-scheduled t)

;; ORG BABEL
(require 'ob-go)
(require 'ob-ocaml)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '(
	(emacs-lisp . nil)
        (ocaml . t)
	(go . t)
	)
      )

;; fontify code in code blocks
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
