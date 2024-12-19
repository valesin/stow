(use-package org
  :ensure t  ; Make sure org-mode is installed
  :diminish org-cdlatex-mode
  :diminish org-indent-mode
  :bind  ; Global keybindings for org-mode functions
  (("C-c l" . org-store-link)    ; Store a link to the current location
   ("C-c a" . org-agenda)        ; Open the org agenda view
   ("C-c c" . org-capture)       ; Start org capture
   ("C-c s" . org-anki-sync-entry)  ; Sync current entry with Anki
   )
  :init
  (defun org-refile-candidates ()
    (directory-files-recursively "~/Documents/Personal/Notes/Uni" "^[[:alnum:]].*\\.org\\'"))
  
  :custom
  (org-directory "~/Documents/")  ; Base directory for org files
  (org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELLED(c)" "FUTURE(f)")))  ; Custom TODO states workflow
  (org-agenda-files '("~/Documents/Personal/Actions/" 
                     "~//Documents/Personal/inbox.org"))  ; Files to be included in agenda view
  (org-hide-emphasis-markers t)   ; Hide markup symbols like *bold* /italic/
  (org-indent-indentation-per-level 4)  ; Number of spaces for each level of indentation
  (org-startup-indented t)        ; Enable org-indent-mode by default
  (org-special-ctrl-a/e t)        ; Smart home/end keys in org mode
  (org-special-ctrl-k t)          ; Smart kill line in org mode
  (org-log-done t)               ; Add timestamp when marking items as DONE
  (org-M-RET-may-split-line nil) ; Prevent M-RET from splitting lines
  (org-src-fontify-natively t)   ; Syntax highlighting in source blocks
  (org-id-link-to-org-use-id t)  ; Use IDs for linking between entries

  ;; Capture templates for different types of notes
  (org-capture-templates
        '(("t" "todo" entry  ; Quick TODO entries
           (file "~/Documents/Personal/inbox.org")
           "* TODO %?\nFrom: %a\n")
          
          ("i" "info to process" entry  ; General information entries
           (file "~/Documents/Personal/inbox.org")
           "* %? \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\nFrom: %a\n")
          
          ("j" "journal" entry  ; Journal entries with timestamp
           (file+datetree "~/Documents/Personal/journal.org")
           "* %^{Title}\t%^g \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED:%U\n:END:\n%?\n")
          
          ("r" "references")  ; Parent template for references
          
          ("rw" "bookmarks" entry  ; Web bookmarks
           (file+headline "~/Documents/Personal/Reference/references.org" "Bookmarks")
           "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")
          
          ("rb" "books" entry  ; Book references
           (file+headline "~/Documents/Personal/Reference/references.org" "Books")
           "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")
          
          ("rf" "feed" entry  ; RSS feed entries
           (file+headline "~/Documents/Personal/Reference/rssfeeds.org" "Uncategorized")
           "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n")
	  
	  ("a" "anki")

	  ("aa" "algoritmi" entry  ; Book references
           (file "~/Documents/Personal/Reference/Anki/anki_algoritmi.org")
           "\n* %^{Front}      %^g\n%?\n"
	   :after-finalize (lambda () 
			     (org-anki-sync-all)))
	  
	  
	  ("ar" "reti" entry  ; Book references
	   (file "~/Documents/Personal/Reference/Anki/anki_reti.org")
	   "\n* %^{Front}      %^g\n%?\n"
	   :after-finalize (lambda () 
			     (org-anki-sync-all)))
	  )
	)
  )

  
  
  ;; Refile settings
  (org-refile-targets '(
			(org-agenda-files :maxlevel . 5)  ; Allow refiling to level 5 in agenda files
                        (nil :maxlevel . 10)
			(org-refile-candidates :maxlevel . 3)
			)
		      )            ; And to level 10 in current buffer
  (org-refile-use-outline-path 'file)  ; Show file names in refile interface
  
  ;; Babel settings
  (org-confirm-babel-evaluate nil)  ; Don't ask for confirmation before executing code blocks
  
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

  :config
   ;; Configure org-babel languages for code block execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)  ; Support for Emacs Lisp
     (ocaml . t)         ; Support for OCaml
     (go . t)            ; Support for Go
     (latex . t))))      ; Support for LaTeX

(use-package htmlize
  :ensure t  ; Package for converting org-mode buffers to HTML with syntax highlighting
  )

(use-package cdlatex
  :ensure t  ; Fast math input in LaTeX and org-mode
  :hook (org-mode . turn-on-org-cdlatex))  ; Enable CDLaTeX in org-mode

(use-package ob-go
  :ensure t)  ; org-babel support for Go programming language

;;(use-package ob-ocaml
;;  :ensure t)  ; org-babel support for OCaml programming language

(use-package org-web-tools
  :ensure t)  ; Tools for handling web content in org-mode

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

(use-package org-anki
  :ensure t)

