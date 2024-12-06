;; This folder and its subfolder, contain every function needed for the setup
;; The files are loaded individually
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
 
(load "global_config.el")
(load "visual_config.el")
(load "org_config.el")
(load "org_roam_config.el")
;;(load "programming_config.el")
;;(load "ocaml_config.el")
;;(load "go_config.el")
;;(load "elfeed_config.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ledger diminish ob-ocaml zotxt yasnippet tuareg org-web-tools org-roam org-drill org-caldav org-anki ob-go lsp-ui ledger-mode htmlize helm go-mode elfeed-org consult company cdlatex auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
