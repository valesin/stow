(use-package org
  :diminish org-cdlatex-mode
  :diminish org-indent-mode
  :diminish olivetti-mode
  
  :bind  ; Global keybindings for org-mode functions
  (("C-c l" . org-store-link)    ; Store a link to the current location
   ("C-c a" . org-agenda)        ; Open the org agenda view
   ("C-c c" . org-capture)       ; Start org capture
   ("C-c s" . org-anki-sync-entry)  ; Sync current entry with Anki
   )

  :hook ((org-mode . visual-line-mode))

  :custom
  (org-element-use-cache nil) ;; To avoid...
  (org-element-cache-persistent nil) ;; issues with warning...
  (org-directory "~/Documents/")  ; Base directory for org files

  ;; Feeds
  (org-feed-alist
      '(("Marginalian"
         "https://www.themarginalian.org/feed/"
         "~/Documents/feeds.org" "Marginalian entries")))
  
  ;; Visual
  (org-hide-emphasis-markers t)   ; Hide markup symbols like *bold* /italic/
  (org-indent-indentation-per-level 4)  ; Number of spaces for each level of indentation
  (org-startup-indented t)        ; Enable org-indent-mode by default
  (org-hide-leading-stars t)
  (org-auto-align-tags t)
  (org-tags-column -80)
  (org-fold-catch-invisible-edits     'show-and-error)
  (org-startup-folded 'show2levels)
  (org-format-latex-options
   '(
     :foreground default
		 :background default
		 :scale 1.0
		 :html-foreground "Black"
		 :html-background "Transparent"
		 :html-scale 1.0
		 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[") :scale 1.35))

  
  ;; Behaviour
  (org-special-ctrl-a/e t)        ; Smart home/end keys in org mode
  (org-special-ctrl-k t)          ; Smart kill line in org mode
  (org-M-RET-may-split-line nil) ; Prevent M-RET from splitting lines
  (org-id-link-to-org-use-id 'create-if-interactive)  ; Use IDs for linking between entries

  ;; Archiving
  (org-archive-location "%s_archive.gpg::")
  ;; TODOs
  (org-todo-keywords
           '((sequence "TODO(t)" "NEXT(n)""WAIT(w)" "|" "DONE(d)")
             (sequence "APPT(a)" "|" "OK(o)")
             (sequence "|" "CANCELLED")))
  (org-use-fast-todo-selection t) ; Changing a task state is done with C-c C-t KEY 
  (org-treat-S-cursor-todo-selection-as-state-change nil) ; Skip processing when switching todo keywords with S-left/right
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-log-done 'time) ; Add timestamp when marking items as DONE
  
  ;; Agenda
  (org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
  (org-agenda-files '(
		      "~/Documents/Personal/todo.org.gpg"
		      "~/Documents/Personal/Calendar/calendar.org.gpg"
		      ))  ; Files to be included in agenda view
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-custom-commands
   '(("a" "Calendar, Next and Todo"
      ((agenda "")
       (todo "NEXT")
       (todo "TODO"))
      ))
   )
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 14)
  (org-agenda-window-setup 'current-window)
  (org-deadline-warning-days 15)
;; Agenda styling
   (org-agenda-block-separator ?─)
   (org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
   (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  ;; Capture templates for different types of notes
  (org-capture-templates
        '(("t" "todo" entry  ; Quick TODO entries
           (file+olp "~/Documents/Personal/todo.org.gpg" "Tasks" "Uncategorized" "Tasks")
           "* TODO %?\n")
          
          ("i" "info to process" entry  ; General information entries
           (file+olp "~/Documents/Personal/todo.org.gpg" "Info")
           "* %? \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\nFrom: %a\n")
          
          ("j" "journal" entry  ; Journal entries with timestamp
           (file+olp+datetree "~/Documents/Personal/journal.org.gpg")
           "* %^{Title}\t%^g \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED:%U\n:END:\n%?\n")
          
          ("r" "references")  ; Parent template for references
          
          ("rw" "bookmarks" entry  ; Web bookmarks
           (file+headline "~/Documents/Personal/Reference/references.org.gpg" "Bookmarks")
           "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")
          
          ("rb" "books" entry  ; Book references
           (file+headline "~/Documents/Personal/Reference/references.org.gpg" "Books")
           "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")
          
          ("rf" "feed" entry  ; RSS feed entries
           (file+headline "~/Documents/Personal/Reference/rssfeeds.org.gpg" "Uncategorized")
           "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n")
	  
	  ("a" "anki")

	  ("aa" "algoritmi" entry  ; Algoritmi anki
	   (file "~/Documents/Personal/Reference/Anki/anki_algoritmi.org.gpg")
	   "\n* %^{Front}      %^g\n%?\n"
	   :jump-to-captured t
	  )
	 
	  ("ar" "reti" entry  ; Book references
	   (file "~/Documents/Personal/Reference/Anki/anki_reti.org.gpg")
	   "\n* %^{Front}      %^g\n%?\n"
	   :jump-to-captured f ;; poi risettare a true eventualmente (a meno che non treovo un modo automatico per fare la push su anki
	  )
	  )
	)
  
  ;; Refile settings
  (org-refile-targets '(
			(org-agenda-files :maxlevel . 5)  ; Allow refiling to level 5 in agenda files
                        (nil :maxlevel . 10) ;; And to level 10 in current buffer
			))
;;(org-refile-candidates :maxlevel . 3) ;; Add the whole Roam directory to refile. Maybe in the future.

  (org-refile-use-outline-path 'file)  ; Show file names in refile interface
  
  ;; Babel settings
  (org-confirm-babel-evaluate nil)  ; Don't ask for confirmation before executing code blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)

  ;; Exporting settings
  (org-export-headline-levels 10) ;; headlines exported as they are until 10th level
  (org-export-with-broken-links 'mark) 
  ;; Publishing settings
  (org-publish-project-alist
   '(("Uni"  ; Project name for university notes
      :publishing-function org-html-publish-to-html  ; Convert org to HTML
      :base-directory "~/Documents/Personal/Notes/Uni/"  ; Where org files are
      :publishing-directory "~/Public/uni_notes"  ; Where HTML files go
      :language it
      :recursive t        ; Include subdirectories
      :with-creator nil   ; Don't show "Created by Org" footer
      :with-author nil    ; Don't show author name
      :section-numbers nil  ; Don't number sections
      :with-toc t          ; Include table of contents
      :auto-sitemap t
      :htmlized-source t   ; Include source code with syntax highlighting
      :auto-sitemap t      ; Generate sitemap.org automatically
      :with-latex t        ; Enable LaTeX math support
      :html-validation-link nil  ; Remove validation link
      :html-head-include-scripts nil     ; Don't include default scripts
      :html-head-include-default-style nil  ; Don't include default CSS
      :html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" onerror=\"this.onerror=null;this.href='local.css';\" />"  ; Use simple CSS framework
      :with-broken-links 'mark
      )))  ; Mark broken links instead of failing
  
  ;; MathJax settings for LaTeX math rendering
  (org-html-mathjax-options
   '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")  ; MathJax CDN path
     (scale 1.0)         ; Default math scale
     (align "center")    ; Center align equations
     (font "mathjax-modern")  ; Modern math font
     (overflow "scale")  ; How to handle overflow
     (tags "ams")       ; Use AMS math environment
     (indent "0em")     ; No indent for equations
     (multlinewidth "85%")  ; Width for multiline equations
     (tagindent ".8em")    ; Indent for equation tags
     (tagside "right")))   ; Place tags on right side
  
  ;; MathJax configuration template
  (org-html-mathjax-template  ; Custom MathJax configuration
   "<script>
    window.MathJax = {
      tex: {
        ams: { multlineWidth: '%MULTLINEWIDTH' },
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
    </script>
    <script id=\"MathJax-script\" async src=\"%PATH\"></script>")

  ;; CITE
  (org-cite-global-bibliography
   '("~/Documents/Books/bib.json"))
  
  (org-cite-export-processors
   '((latex biblatex)                         
     (html . (csl "~/Zotero/styles/ieee.csl")))) 

  :custom-face
  (org-document-title ((t (:foreground "midnight blue"
                                       :weight bold
                                       :height 1.44))))
  :config
  ;; Configure org-babel languages for code block execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)  ; Support for Emacs Lisp
     (ocaml . t)         ; Support for OCaml
     (go . t)            ; Support for Go
     (latex . t)))       ; Support for LaTeX
  
(defun my/prettify-symbols-setup ()
  ;; Checkboxes
  ;; (push '("[ ]" . "") prettify-symbols-alist)
  ;; (push '("[X]" . "") prettify-symbols-alist)
  ;; (push '("[-]" . "" ) prettify-symbols-alist)

  ;; org-babel
  ;; (push '("#+BEGIN_SRC" . ≫) prettify-symbols-alist)
  ;; (push '("#+END_SRC" . ≫) prettify-symbols-alist)
  ;; (push '("#+begin_src" . ≫) prettify-symbols-alist)
  ;; (push '("#+end_src" . ≫) prettify-symbols-alist)
  

  (push '("#+BEGIN_QUOTE" . ❝) prettify-symbols-alist)
  (push '("#+END_QUOTE" . ❞) prettify-symbols-alist)

  ;; Drawers
  ;;(push '(":PROPERTIES:" . "") prettify-symbols-alist)

  ;; Tags
  ;; (push '(":projects:" . "") prettify-symbols-alist)
  ;; (push '(":work:"     . "") prettify-symbols-alist)
  ;; (push '(":inbox:"    . "") prettify-symbols-alist)
  ;; (push '(":task:"     . "") prettify-symbols-alist)
  ;; (push '(":thesis:"   . "") prettify-symbols-alist)
  ;; (push '(":uio:"      . "") prettify-symbols-alist)
  ;; (push '(":emacs:"    . "") prettify-symbols-alist)
  ;; (push '(":learn:"    . "") prettify-symbols-alist)
  ;; (push '(":code:"     . "") prettify-symbols-alist)

  (prettify-symbols-mode))

  ;; (defun my-auto-commit-org-file ()
  ;; (when (eq major-mode 'org-mode)
  ;;   (shell-command "cd ~/Documents/ && git add . && git commit -m '${LOGNAME} on $(date '+%d/%m/%Y')' && git push origin main")))
  )      

(use-package olivetti
  :straight t
  :hook (org-mode . olivetti-mode))

(use-package org-superstar
  :straight t
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("◉" "○" "‣" "◈" "◇"))
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO"  . 9744)
                                          ("NEXT"  . 9744)
                                          ("WAIT"  . 9744)
					  ("DONE"  . 9745)))
  :hook (org-mode . org-superstar-mode))

(use-package org-appear
  :straight t
  :hook     (org-mode . org-appear-mode)
  :custom
   (org-appear-autoemphasis t)
   (org-appear-autolinks t))

;; DOESNT WORK YET
(use-package org-fragtog
  :hook (org-mode-hook . org-fragtog-mode))

(use-package htmlize
  :straight t
  :ensure t  ; Package for converting org-mode buffers to HTML with syntax highlighting
  )

(use-package cdlatex
  :straight t
  :ensure t  ; Fast math input in LaTeX and org-mode
  :hook (org-mode . turn-on-org-cdlatex))  ; Enable CDLaTeX in org-mode

(use-package ob-go
  :straight t
  :ensure t
  :after (org))  ; org-babel support for Go programming language

;;(use-package ob-ocaml
;;  :ensure t)  ; org-babel support for OCaml programming language

;;(use-package org-web-tools
;;  :ensure t)  ; Tools for handling web content in org-mode

(use-package org-caldav
  :straight t
  :after (org)
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
  (setq org-caldav-calendar-id "Y2FsOi8vMC8xMTA")
   ;; Org filename where new entries from calendar stored
  (setq org-caldav-inbox '(file+olp "~/Documents/Personal/Calendar/calendar.org.gpg" "Appointments"))
  ;; Additional Org files to check for calendar events
  (setq org-caldav-files '("~/Documents/Personal/Calendar/calendar.org.gpg"
                         ;;"~/Documents/Personal/Actions/meetings.org"
                         ;;"~/Documents/Work/Projects/project1.org"
			   ))
  (setq org-caldav-save-directory "~/Documents/Personal/Calendar/")

  ;; I found that the original value (see variable description) is ok and i will use it in case of emergency
  ;;(setq org-caldav-backup-file "~/Documents/Personal/Actions/org-caldav/org-caldav-backup.org")

  :config
  (setq org-icalendar-alarm-time 0)
  ;; This makes sure to-do items as a category can show up on the calendar
  ;;(setq org-icalendar-include-todo t)
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

(use-package org-anki
  :straight t
  :ensure t
  :after (org)
  )

