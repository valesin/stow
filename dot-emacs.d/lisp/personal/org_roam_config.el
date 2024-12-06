;; Last updated: 2024-12-05 20:51:38 UTC

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/Personal/Notes"))        ; Set org-roam directory
  (find-file-visit-truename t)                                            ; Follow symlinks
  (org-roam-database-connector 'sqlite-builtin)                           ; Use built-in SQLite
  (org-roam-completion-everywhere t)                                      ; Enable completion everywhere
  
  :bind  ; Global keybindings for org-roam functions
  (("C-c n r" . org-roam-buffer-toggle)    ; Toggle org-roam buffer
   ("C-c n i" . org-roam-node-insert)      ; Insert org-roam node
   ("C-c n s" . org-id-get-create)         ; Create or get ID
   ("C-c n c" . org-roam-capture)          ; Capture to org-roam
   ("C-c n f" . org-roam-node-find)        ; Find org-roam node
   ("C-c n n" . org-roam-node-find))       ; Alternative find binding
  
  :custom  ; Capture templates
  (org-roam-capture-templates
   '(("m" "main" plain                      ; Main note template
      "#+filetags: %^g\n%?"
      :if-new (file+head "Main/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     
     ("u" "uni")                            ; University parent template
     
     ("ua" "algoritimi" plain               ; Algorithms template
      "#+ANKI_DECK: Algoritmi\n#+filetags: :algoritmi:%^g\n%?"
      :if-new (file+head "Uni/Algoritmi/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     
     ("ur" "reti" plain                     ; Networks template
      "#+ANKI_DECK: Reti\n#+filetags: :reti:%^g\n%?"
      :if-new (file+head "Uni/Reti/${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     
     ("f" "fleeting" plain                  ; Fleeting notes template
      "#+filetags: %^g\n%?"
      :if-new (file+head "Fleeting/${slug}.org"
                         ":PROPERTIES:\n:CREATED: %T\n:REFERRER: %a\n:END:\n#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))
  
  :config
  (org-roam-db-autosync-mode))             ; Enable automatic database sync
