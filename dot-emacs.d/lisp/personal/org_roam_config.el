;; setup org roam directory
(setq org-roam-directory (file-truename "~/Documents/Roam"))
(setq find-file-visit-truename t)

;; setup autosync and select builtin sqlite connector
(org-roam-db-autosync-mode)
(setq org-roam-database-connector 'sqlite-builtin)

;; enable link completion everywhere (don't know what it means)
(setq org-roam-completion-everywhere t)

(global-set-key (kbd "C-c n l") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n s") #'org-id-get-create)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n n") #'org-roam-node-find)
;;(global-set-key (kbd "C-c n I") #'org-roam-node-insert-immediate);; ;; Bind this to C-c n I
;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   (interactive "P")
;;   (let ((args (cons arg args))
;;         (org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;                                                   '(:immediate-finish t)))))
;;     (apply #'org-roam-node-insert args)))

;; this code is copied from https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/

;; show the type of note in the completion
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;; the code above is taken from https://jethrokuan.github.io/org-roam-guide/

;; li cancello tutti perchè le references possono essere realizzate con il capture normale
;; questo non mi piace perchè inverte l'ordine senza un motivo apparente, inoltre tutte le funzioni che mi fornisce sono standard di org, compreso l'id
;; utilizzo la versione standard
;; (setq org-roam-capture-templates
;;       '(
	
;; 	("d" "default" plain
;; 	 "%?"
;; 	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;; 	 :unnarrowed t)
	
;; 	("r" "references")

;; 	("rb" "book" plain
;; 	 "* Source\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
;; 	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;; 	 :unnarrowed t
;; 	 :immediate-finish t)

;; 	("rw" "book nots" entry
;; 	 "* Source\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
;; 	 :if-new (file+head+olp "bookprova.org" "#+title: ${title}\n" ("booknots") )
;; 	 :unnarrowed t
;; 	 :immediate-finish t)

;; 	("rg" "bookmarks" entry
;; 	 "* [[%^{Link}][%^{Title}]] %^g\n:PROPERTIES:\n :ID: %(org-id-uuid)\n :DATE_ADDED: %t\n :END:\n %? "
;; 	 :if-new (file+head+olp "references.org" "#+title: ${title}\n" ("Bookmarks") )
;; 	 :unnarrowed t
;; 	 :immediate-finish t)
;; 	)
;;       )

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "Main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "Reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)))
