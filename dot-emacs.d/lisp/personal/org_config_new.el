(use-package org
  :ensure t
  
  :bind
	("C-c l" . org-store-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c s" . org-anki-sync-entry)
	
  :hook
	(org-mode . visual-line-mode)  ; Enable word wrap
	(org-mode . org-indent-mode)   ; Enable indentation
	(org-mode . turn-on-org-cdlatex)  ; Enable cdlatex for latex snippets previews

	:config
	(defun org-refile-candidates () (directory-files-recursively "~/Documents/Notes/Uni" "^[[:alnum:]].*\\.org\\'"))
(add-to-list 'org-refile-targets '(org-refile-candidates :maxlevel . 3))

;; latex


	
  :custom
	  (org-directory "~/Documents/")
          (org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "APPT" "|" "DONE" "CANCELLED" "DEFERRED")))
	  (org-agenda-files '("/home/vjo/Documents/Personal/Planner/" "/home/vjo/Documents/Personal/Notes/" ))
	  (org-hide-emphasis-markers t) ;; hide emphasis markers (*..*, /../)
	  (org-indent-indentation-per-level 4) ;; setup indentation space
	  (org-startup-indented t) ;; indent on startup
	  (org-hide-emphasis-markers t) ;;Non-nil means font-lock should hide the emphasis marker characters.
	  (org-special-ctrl-a/e t) ;;C-a and C-e bring to beginning of item first (after stars and TODO), then beginning of line
	  (org-special-ctrl-k t) ;; Like before
	  (org-log-done t) ;;   log starting time for TODO lists
	  (org-M-RET-may-split-line nil) 	  ;;   M-Ret creates and goes to a new blank line under the current
	  (org-src-fontify-natively t) ;; fontify exports
	  ;; Create ID to support a link when capturing from a source
	  (org-id-link-to-org-use-id create-if-interactive)	  
	  (org-capture-templates
	   '(("t" "todo" entry
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
	  (org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 10 )))
	  (org-refile-use-outline-path 'file)

	  ;; Babel
	  (org-babel-do-load-languages
	   'org-babel-load-languages
	   '(
	     (emacs-lisp . nil)
             (go . t)
	     (latex .t)
	     )
	   )
	  
;; don't ask confirm on C-c C-c
(setq org-confirm-babel-evaluate nil)


;;add a recursively scanned directory as target

	    
)
