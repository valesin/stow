;; This folder and its subfolder, contain every function needed for the setup
;; The files are loaded individually
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(load "global_config.el")
(load "visual_config.el")
(load "org_config.el")
(load "programming_config.el")
(load "ocaml_config.el")
(load "go_config.el")


