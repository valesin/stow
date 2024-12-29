(use-package elfeed
  :ensure t   ; Make sure elfeed is installed
  :bind      ; Global keybinding for elfeed
  ("C-x w" . elfeed))   ; Quick access to RSS reader

(use-package elfeed-org
  :ensure t    ; Make sure elfeed-org is installed
  :after elfeed ; Load after elfeed is loaded
  :config     ; Configuration to run after loading
  (elfeed-org)   ; Initialize elfeed-org
  (setq rmh-elfeed-org-files (list "~/Documents/Personal/Reference/rssfeeds.org.gpg")))   ; Set RSS feeds org file
