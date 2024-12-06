(use-package diminish :ensure t)
(diminish 'visual-line-mode)

;;Global settings
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-16"))
(setq line-spacing 0.1)          ; Set line spacing
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq visible-bell t)            ; Flash when the bell rings
(set-face-attribute 'default nil :height 160); Change font size
(menu-bar-mode -1) ; Hide menu bar
(tool-bar-mode -1) ; Hide tool bar
(scroll-bar-mode -1) ; Hide scroll bar

;; Don't use audible bells, use visual bells. 40
(setq ring-bell-function 'ignore)
(setq visible-bell t)
