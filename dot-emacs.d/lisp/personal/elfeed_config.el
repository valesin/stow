(global-set-key (kbd "C-x w") 'elfeed)

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration.
(setq rmh-elfeed-org-files (list "~/Documents/Reference/rssfeeds.org"))
