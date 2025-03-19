(use-package org
  :diminish org-cdlatex-mode
  :diminish org-indent-mode
  :diminish olivetti-mode
  :diminish org-cdlatex-mode
  
  :bind  ; Global keybindings for org-mode functions
  (("C-c l" . org-store-link)    ; Store a link to the current location
   ("C-c a" . org-agenda)        ; Open the org agenda view
   ("C-c c" . org-capture)       ; Start org capture
   ("C-c s" . org-anki-sync-entry)  ; Sync current entry with Anki
   )

  :hook ((org-mode . visual-line-mode))

  :custom
  (org-element-use-cache nil) ;; To avoid...
  (org-element-cache-persistent nil) ;; issues with warning
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
  (org-tags-column -60)
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
   (org-my-anki-file "~/Documents/Personal/Reference/anki.org.gpg")
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
      ("ab" "basic")
      ("ac" "cloze")
      
      ("c" "calendar")
      
      ("cs" "scheduled" entry 
       (file+headline "~/Documents/Personal/Calendar/calendar.org.gpg" "Appointments")
       "\n* %^{Title}\nSCHEDULED: %^T"
       :immediate-finish t
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
  (org-export-exclude-tags '("anki" "noexport" "private"))
  ;; Publishing settings
  (org-publish-project-alist
   '(("Uni"  ; Project name for university notes
      :publishing-function org-html-publish-to-html  ; Convert org to HTML
      :base-directory "~/Documents/Personal/Notes/Uni/"  ; Where org files are
      :base-extension "org.gpg"
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
   '("~/Library/bib.json"))
  
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
     (latex . t)
     (sql . t)
     (gnuplot . t)
     ))       ; Support for LaTeX
  
  ;; Define the Anki file location
  (setq org-my-anki-file "~/Documents/Personal/Reference/anki.org.gpg")
  
  (defun my/org-capture--build-template (template-key topic note-type)
    "Helper function to construct a capture template.
TEMPLATE-KEY is used by org capture as the key.
TOPIC is used as both the headline in the Anki file and the target deck name.
NOTE-TYPE should be a string like \"Basic\" or \"Cloze\".
The deck is now derived automatically from TOPIC."
    (list template-key
          topic
          'entry
          `(file+headline org-my-anki-file ,topic)
          (concat "* %<%H:%M>   %^g\n"
                  ":PROPERTIES:\n"
                  ":ANKI_NOTE_TYPE: " note-type "\n"
                  ":END:\n"
                  "** Front\n%?\n"
                  "** Back\n%x\n")))
  
  (defun my/org-capture-add-template (key topic)
    "Add Org capture templates for Anki using KEY and TOPIC.
KEY is a string used as a prefix for the capture template keys;
it will have \"ab\" appended for a Basic note and \"ac\" for a Cloze note.
TOPIC is used as the headline as well as the deck name.
This function always adds both Basic and Cloze templates."
    (add-to-list 'org-capture-templates
		 (my/org-capture--build-template (concat key "ab") topic "Basic"))
    (add-to-list 'org-capture-templates
		 (my/org-capture--build-template (concat key "ac") topic "Cloze")))
  
  (my/org-capture-add-template "S" "Statistica")
  
  ;; Example Usage:
  
  ;; Without specifying the deck (uses "All" for generic and TOPIC for topic-specific)
  ;;(my/org-capture-add-template "a" "Programming")       ; Uses deck "All"
  ;;(my/org-capture-add-template "b" "Math")              ; Uses deck "All"
  ;;(my/org-capture-add-template "c" "History")           ; Uses deck "All"
  ;;(my/org-capture-add-template "a" "Languages" nil t)     ; Uses deck "Languages" because topic-specific is true
  
  ;; Specifying the deck explicitly (overrides the default behavior)
  ;;(my/org-capture-add-template "a" "Philosophy" "CustomDeck") ; Uses deck "CustomDeck"
  ;;(my/org-capture-add-template "b" "Science" "MyScienceDeck")  ; Uses deck "MyScienceDeck"
  )

