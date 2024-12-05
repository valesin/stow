(require 'org)
(require 'htmlize)
(require 'cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
;; Global commands for org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c s") #'org-anki-sync-entry)

; set org path
(setq org-directory "~/Documents/")
;set stages
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "APPT" "|" "DONE" "CANCELLED" "DEFERRED")))

;;set files to fill agenda
(setq org-agenda-files '("/home/vjo/Documents/Personal/Planner/" "/home/vjo/Documents/Personal/Notes/" ))
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
;; Follow the links (NO, use C-c C-o instead)
;;(setq org-return-follows-link  t)
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
;; Create ID to support a link when capturing from a source
(setq org-id-link-to-org-use-id t)

(setq org-capture-templates
      '(
	
	("t" "todo" entry
	 (file "~/Documents/Personal/inbox.org")
         "* TODO %?\nFrom: %a\n")

	("i" "info to process" entry
	 (file "~/Documents/Personal/inbox.org")
	 "* %? \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\nFrom: %a\n")

	("j" "journal" entry
	 (file+datetree "~/Documents/Personal/journal.org")
         "* %^{Title}\t%^g \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED:%U\n:END:\n%?\n")

	("r" "References")

	("rw" "bookmarks" entry
	 (file+headline "~/Documents/Personal/Reference/references.org" "Bookmarks")
	 "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")

        ("rb" "books" entry
	 (file+headline "~/Documents/Personal/Reference/references.org" "Books")
	 "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")

      	("rf" "feed" entry
	 (file+headline "~/Documents/Personal/Reference/rssfeeds.org" "Uncategorized")
	 "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n")
	)
)

;; use whole path to refile
;;(setq org-refile-use-outline-path t)
;; Refile until 5 level and use file name, to have 1 level headlines

(setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 10 )) )
(setq org-refile-use-outline-path 'file)

;;add a recursively scanned directory as target
(defun org-refile-candidates () (directory-files-recursively "~/Documents/Notes/Uni" "^[[:alnum:]].*\\.org\\'"))
(add-to-list 'org-refile-targets '(org-refile-candidates :maxlevel . 3))

;; latex


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
	(latex .t)
	)
      )

;; don't ask confirm on C-c C-c
(setq org-confirm-babel-evaluate nil)

;; PUBLISH
(setq org-publish-project-alist
      '(
	(
	 "Uni"
	 :publishing-function org-html-publish-to-html
         :base-directory "/home/vjo/Documents/Notes/Uni/"
         :publishing-directory "/home/vjo/Public/uni_notes"
	 :recursive t
	 
	 :with-creator nil
	 :with-author nil
	 :section-numbers nil
         :with-toc t
	 :htmlized-source t
	 :auto-sitemap t
	 :with-latex t
	 :html-validation-link nil
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" onerror=\"this.onerror=null;this.href='local.css';\" />"
	 :with-broken-links 'mark
	 )
	)
      )

(setq org-html-mathjax-options
      '(
       (path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
       (scale 1.0)
       (align "center")
       (font "mathjax-modern")
       (overflow "scale")
       (tags "ams")
       (indent "0em")
       (multlinewidth "85%")
       (tagindent ".8em")
       (tagside "right")
       )
      )

(setq org-html-mathjax-template
      "<script>
  window.MathJax = {
    tex: {
      ams: {
        multlineWidth: '%MULTLINEWIDTH'
      },
      {packages: {'[+]': ['mathtools']}},
      tags: '%TAGS',
      tagSide: '%TAGSIDE',
      tagIndent: '%TAGINDENT'
    },
    chtml: {
      scale: %SCALE,
      displayAlign: '%ALIGN',
      displayIndent: '%INDENT'
    },
    svg: {
      scale: %SCALE,
      displayAlign: '%ALIGN',
      displayIndent: '%INDENT'
    },
    output: {
      font: '%FONT',
      displayOverflow: '%OVERFLOW'
    },
    loader: {
      load: ['[tex]/mathtools']
    },
};
  };
</script>

<script
  id=\"MathJax-script\"
  async
  src=\"%PATH\">
</script>"
      )

;;org anki


(use-package org-caldav
  :init
  ;; This is the sync on close function; it also prompts for save after syncing so 
  ;; no late changes get lost 
;;  (defun org-caldav-sync-at-close ()
;;    (org-caldav-sync)
;;    (save-some-buffers))
  
  ;; This is the delayed sync function; it waits until emacs has been idle for 
  ;; "secs" seconds before syncing.  The delay is important because the caldav-sync
  ;; can take five or ten seconds, which would be painful if it did that right at save.  
  ;; This way it just waits until you've been idle for a while to avoid disturbing 
  ;; the user.
  (defvar org-caldav-sync-timer nil
     "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
  (defun org-caldav-sync-with-delay (secs)
    (when org-caldav-sync-timer
      (cancel-timer org-caldav-sync-timer))
    (setq org-caldav-sync-timer
	  (run-with-idle-timer
	   (* 1 secs) nil 'org-caldav-sync)))
  
  ;; Actual calendar configuration edit this to meet your specific needs
  (setq org-caldav-url "https://dav.mailbox.org/caldav/")
  (setq org-caldav-calendars
	'(
	  (:calendar-id "Y2FsOi8vMC8xMTA"
	    		:files ("~/Documents/Planner/calendar.org") ;;poi virgola sernza parentesi sito,sito,sito
			:inbox "~/Documents/Planner/inbox.org")
	  )
	)

  (setq org-caldav-backup-file "~/Documents/Personal/Planner/org-caldav/org-caldav-backup.org")
  (setq org-caldav-save-directory "~/Documents/Personal/Planner/org-caldav/")
  
  :config
  (setq org-icalendar-alarm-time 1)
  ;; This makes sure to-do items as a category can show up on the calendar
  (setq org-icalendar-include-todo t)
  ;; This ensures all org "deadlines" show up, and show up as due dates
  (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
  ;; This ensures "scheduled" org items show up, and show up as start times
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  ;; Add the delayed save hook with a five minute idle timer
  ;; (add-hook 'after-save-hook
  ;; 	    (lambda ()
  ;; 	      (when (eq major-mode 'org-mode)
  ;; 		(org-caldav-sync-with-delay 300))))
  ;; ;; Add the close emacs hook
  ;;(add-hook 'kill-emacs-hook 'org-caldav-sync-at-close)
  )

(require 'org-web-tools)

;; don't do it cause i call it from the script
;;(defun org-caldav-sync-at-close ()
;;  (org-caldav-sync)
;;  (save-some-buffers))

;; (defun org-html--reference (datum info &optional named-only)
;;   "Return an appropriate reference for DATUM.
;; DATUM is an element or a `target' type object.  INFO is the
;; current export state, as a plist.
;; When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
;; nil.  This doesn't apply to headlines, inline tasks, radio
;; targets and targets."
;;   (let* ((type (org-element-type datum))
;; 	 (user-label
;; 	  (org-element-property
;; 	   (pcase type
;; 	     ((or `headline `inlinetask) :CUSTOM_ID)
;; 	     ((or `radio-target `target) :value)
;; 	     (_ :name))
;; 	   datum))
;;          (user-label (or user-label
;;                          (when-let ((path (org-element-property :ID datum)))
;;                            (concat "ID-" path)))))
;;     (cond
;;      ((and user-label
;; 	   (or (plist-get info :html-prefer-user-labels)
;; 	       ;; Used CUSTOM_ID property unconditionally.
;; 	       (memq type '(headline inlinetask))))
;;       user-label)
;;      ((and named-only
;; 	   (not (memq type '(headline inlinetask radio-target target)))
;; 	   (not user-label))
;;       nil)
;;      (t
;;       (org-export-get-reference datum info)))))







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
