;; Load required libraries
(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'diminish) ;; if installed

;; Global keybindings for org-mode functions
(global-set-key (kbd "C-c l") 'org-store-link)    ;; Store a link to the current location
(global-set-key (kbd "C-c a") 'org-agenda)        ;; Open the org agenda view
(global-set-key (kbd "C-c c") 'org-capture)       ;; Start org capture
(global-set-key (kbd "C-c s") 'org-anki-sync-entry) ;; Sync current entry with Anki

;; Diminish minor modes if available
(with-eval-after-load 'org
  (when (fboundp 'diminish)
    (diminish 'org-cdlatex-mode)
    (diminish 'org-indent-mode)
    (diminish 'olivetti-mode)
    (diminish 'org-cdlatex-mode)))  ;; note: duplicate call kept from original config

;; Hook: enable visual-line-mode in org-mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; Custom Settings

;; General org settings
(setq org-element-use-cache nil)
(setq org-element-cache-persistent nil)
(setq org-directory "~/Documents/")

;; Feeds
(setq org-feed-alist
      '(("Marginalian"
         "https://www.themarginalian.org/feed/"
         "~/Documents/feeds.org" "Marginalian entries")))

;; Visual settings for org-mode
(setq org-hide-emphasis-markers t)         ;; Hide markup symbols like *bold* /italic/
(setq org-indent-indentation-per-level 4)    ;; Spaces per indent level
(setq org-startup-indented t)                ;; Enable org-indent-mode by default
(setq org-hide-leading-stars t)
(setq org-auto-align-tags t)
(setq org-tags-column -60)
(setq org-fold-catch-invisible-edits 'show-and-error)
(setq org-startup-folded 'show2levels)
(setq org-format-latex-options
      '(:foreground default
                    :background default
                    :scale 1.0
                    :html-foreground "Black"
                    :html-background "Transparent"
                    :html-scale 1.0
                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
                    :scale 1.35))

;; Behavior settings
(setq org-special-ctrl-a/e t)      ;; Smart home/end keys in org-mode
(setq org-special-ctrl-k t)        ;; Smart kill line in org-mode
(setq org-M-RET-may-split-line nil)  ;; Prevent M-RET from splitting lines
(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-archive-location "%s_archive.gpg::")
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)")
        (sequence "APPT(a)" "|" "OK(o)")
        (sequence "|" "CANCELLED")))
(setq org-use-fast-todo-selection t) 
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-done 'time)  ;; Add timestamp when marking items as DONE

;; Agenda config
(setq org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
(setq org-agenda-files '("~/Documents/Personal/todo.org.gpg"
                         "~/Documents/Personal/Calendar/calendar.org.gpg"))
(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-agenda-custom-commands
      '(("a" "Calendar, Next and Todo"
         ((agenda "")
          (todo "NEXT")
          (todo "TODO")))))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 14)
(setq org-agenda-window-setup 'current-window)
(setq org-deadline-warning-days 15)
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setq org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────")

;; Capture Templates and Anki integration
(setq org-my-anki-file "~/Documents/Personal/Reference/anki.org.gpg")
(setq org-capture-templates
      '(("t" "todo" entry
         (file+olp "~/Documents/Personal/todo.org.gpg" "Tasks" "Uncategorized" "Tasks")
         "* TODO %?\n")
        ("i" "info to process" entry
         (file+olp "~/Documents/Personal/todo.org.gpg" "Info")
         "* %? \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\nFrom: %a\n")
        ("j" "journal" entry
         (file+olp+datetree "~/Documents/Personal/journal.org.gpg")
         "* %^{Title}\t%^g \n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED:%U\n:END:\n%?\n")
        ("r" "references")
        ("rw" "bookmarks" entry
         (file+headline "~/Documents/Personal/Reference/references.org.gpg" "Bookmarks")
         "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")
        ("rb" "books" entry
         (file+headline "~/Documents/Personal/Reference/references.org.gpg" "Books")
         "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%?\n")
        ("rf" "feed" entry
         (file+headline "~/Documents/Personal/Reference/rssfeeds.org.gpg" "Uncategorized")
         "\n* [[%^{Link}][%^{Title}]]      %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n")
        ("a" "anki")
        ("ab" "basic")
        ("ac" "cloze")
        ("c" "calendar")
        ("cs" "scheduled" entry
         (file+headline "~/Documents/Personal/Calendar/calendar.org.gpg" "Appointments")
         "\n* %^{Title}\nSCHEDULED: %^T"
         :immediate-finish t)))

;; Refile settings
(setq org-refile-targets '((org-agenda-files :maxlevel . 5)
                           (nil :maxlevel . 10)))
(setq org-refile-use-outline-path 'file)

;; Babel settings
(setq org-confirm-babel-evaluate nil) ;; No confirmation before executing code blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

;; Export settings
(setq org-export-headline-levels 10)
(setq org-export-with-broken-links 'mark)
(setq org-export-exclude-tags '("anki" "noexport" "private"))

;; Publishing settings
(setq org-publish-project-alist
      '(("Uni"
         :publishing-function org-html-publish-to-html
         :base-directory "~/Documents/Personal/Notes/Uni/"
         :base-extension "org.gpg"
         :publishing-directory "~/Public/uni_notes"
         :language it
         :recursive t
         :with-creator nil
         :with-author nil
         :section-numbers nil
         :with-toc t
         :auto-sitemap t
         :htmlized-source t
         :with-latex t
         :html-validation-link nil
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" onerror=\"this.onerror=null;this.href='local.css';\" />"
         :with-broken-links 'mark)))

;; MathJax settings for LaTeX math rendering
(setq org-html-mathjax-options
      '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
        (scale 1.0)
        (align "center")
        (font "mathjax-modern")
        (overflow "scale")
        (tags "ams")
        (indent "0em")
        (multlinewidth "85%")
        (tagindent ".8em")
        (tagside "right")))
(setq org-html-mathjax-template
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

;; CITE configuration
(setq org-cite-global-bibliography '("~/Library/bib.json"))
(setq org-cite-export-processors
      '((latex biblatex)
        (html . (csl "~/Zotero/styles/ieee.csl"))))

;; Custom face for org-document-title
(custom-set-faces
 '(org-document-title ((t (:foreground "midnight blue"
                                       :weight bold
                                       :height 1.44)))))

;; Configure org-babel languages for code block execution
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)  ;; Emacs Lisp disabled
   (latex . t)
   (sql . t)
   (gnuplot . t)))

;; Define helper functions for Anki capture templates
(defun my/org-capture--build-template (template-key topic note-type)
  "Helper function to construct a capture template.
TEMPLATE-KEY is used by org capture as the key.
TOPIC is used as both the headline in the Anki file and the target deck name.
NOTE-TYPE should be a string like \"Basic\" or \"Cloze\".
The deck is now derived automatically from TOPIC."
  (cond
   ((string= note-type "Basic")
    (list template-key
          topic
          'entry
          `(file+headline org-my-anki-file ,topic)
          (concat "* %<%H:%M>   %^g\n"
                  ":PROPERTIES:\n"
                  ":ANKI_NOTE_TYPE: Basic\n"
                  ":END:\n"
                  "** Front\n%?\n"
                  "** Back\n%x\n")))
   ((string= note-type "Cloze")
    (list template-key
          topic
          'entry
          `(file+headline org-my-anki-file ,topic)
          (concat "* %<%H:%M>   %^g\n"
		  ":PROPERTIES:\n"
		  ":ANKI_NOTE_TYPE: Cloze\n"
		  ":END:\n"
		  "** Text\n%x%?\n"
		  "** Extra\n" )))))

(defun my/org-capture-add-template (key topic)
  "Add Org capture templates for Anki using KEY and TOPIC.
KEY is a string used as a prefix for the capture template keys;
it will have \"ab\" appended for the Basic note and \"ac\" for the Cloze note.
TOPIC is used as the headline as well as the deck name.
This function always adds both Basic and Cloze templates."
  (add-to-list 'org-capture-templates
               (my/org-capture--build-template (concat "ab" key) topic "Basic"))
  (add-to-list 'org-capture-templates
               (my/org-capture--build-template (concat "ac" key) topic "Cloze")))

;; Add Anki capture templates for the "Statistica" topic
(my/org-capture-add-template "s" "Statistica")

(use-package olivetti
  :straight t
  :hook (org-mode . olivetti-mode)
  :custom
   (olivetti-body-width 70)
  )

(use-package org-superstar
  :straight t
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("◉" "○" "‣" "◈" "◇"))
  (setq org-superstar-special-todo-items nil) ;; Makes TODO header bullets into boxes FALSE CAUSE THE BULLET IS TOO LARGE
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
;;(use-package org-fragtog
;;  :hook (org-mode-hook . org-fragtog-mode))

(use-package htmlize
  :straight t
  )

(use-package cdlatex
  :straight t
  :hook (org-mode . turn-on-org-cdlatex))  ; Enable CDLaTeX in org-mode

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

;; (use-package anki-editor
;;   :straight (:type git
;;              :host github
;;              :repo "anki-editor/anki-editor"
;;              :branch "main"
;;              :fork (:host github
;; 			  :repo "valesin/anki-editor"))
;;   :bind
;;   (("C-c 0 i" . anki-editor-insert-note)
;;    ("C-c 0 b" . anki-editor-insert-default-note)
;;    ("C-c 0 p" . anki-editor-push-note-at-point)
;;    ("C-c 0 C-p" . anki-editor-push-notes)
;;    )
;;   )

(use-package anki-editor
  :after org
  :bind (:map org-mode-map
              ("C-<tab> a c" . anki-editor-cloze-region-auto-incr)
              ("C-<tab> a C" . anki-editor-cloze-region-dont-incr)
              ("C-<tab> a 0" . anki-editor-reset-cloze-number)
              ("C-<tab> a i" . anki-editor-insert-note)
	      ("C-<tab> a b" . anki-editor-insert-default-note)
	      ("C-<tab> a p" . anki-editor-push-note-at-point)
	      ("C-<tab> a C-p" . anki-editor-push-notes)
	      )
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

(use-package org-web-tools
  :after org
  :defer
  :bind
  (("C-c 0 l" . org-web-tools-insert-link-for-url)
   ("C-c 0 w" . org-web-tools-insert-web-page-as-entry)
   )
  )

;; (defun my/prettify-symbols-setup ()
;;     ;; Checkboxes
;;     ;; (push '("[ ]" . "") prettify-symbols-alist)
;;     ;; (push '("[X]" . "") prettify-symbols-alist)
;;     ;; (push '("[-]" . "" ) prettify-symbols-alist)
    
;;     ;; org-babel
;;     ;; (push '("#+BEGIN_SRC" . ≫) prettify-symbols-alist)
;;     ;; (push '("#+END_SRC" . ≫) prettify-symbols-alist)
;;     ;; (push '("#+begin_src" . ≫) prettify-symbols-alist)
;;     ;; (push '("#+end_src" . ≫) prettify-symbols-alist)
    
    
;;     (push '("#+BEGIN_QUOTE" . ❝) prettify-symbols-alist)
;;     (push '("#+END_QUOTE" . ❞) prettify-symbols-alist)
    
;;     ;; Drawers
;;     ;;(push '(":PROPERTIES:" . "") prettify-symbols-alist)
    
;;     ;; Tags
;;     ;; (push '(":projects:" . "") prettify-symbols-alist)
;;     ;; (push '(":work:"     . "") prettify-symbols-alist)
;;     ;; (push '(":inbox:"    . "") prettify-symbols-alist)
;;     ;; (push '(":task:"     . "") prettify-symbols-alist)
;;     ;; (push '(":thesis:"   . "") prettify-symbols-alist)
;;     ;; (push '(":uio:"      . "") prettify-symbols-alist)
;;     ;; (push '(":emacs:"    . "") prettify-symbols-alist)
;;     ;; (push '(":learn:"    . "") prettify-symbols-alist)
;;     ;; (push '(":code:"     . "") prettify-symbols-alist)

;;     (prettify-symbols-mode))


(defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "org-capture") (window-system . x)))
    (select-frame-by-name "org-capture")
    (counsel-org-capture)
    (delete-other-windows)
    )
