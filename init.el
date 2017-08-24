;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-
;; Copyright (c) 2011-2017, Nikolay Blokhin
;; All rights reserved.

;;; Commentary:
;; Do not forget to feed cats.

;;; Code:

;; General repository for packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Path to my libs and scripts
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/libs/")
(add-to-list 'load-path "~/.emacs.d/scripts/")

;; list the packages you want
(setq package-list
      '(python-mode ido sr-speedbar paren auto-complete flycheck move-text browse-kill-ring))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))


;;(require 'ido)
;;(require 'python-mode)
;;(require 'sr-speedbar)
;;(require 'paren)


(ac-config-default)               ;; turn on auto-complete
(global-flycheck-mode)            ;; turn on syntax checking
(ido-mode t)                      ;; turn on ido-mode
(set-default 'truncate-lines t)   ;; turn off wrapping for long lines


;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.


(custom-set-variables
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(require-final-newline t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 )

;; Hide Emacs menu sting
(menu-bar-mode -1)
(global-hl-line-mode +1)

;; see matching pairs of parentheses and other characters
(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#d700d7")
    (set-face-attribute 'show-paren-match nil :weight 'extra-bold)


;; Show the column and line numbers
(setq line-number-mode t)
(setq column-number-mode t)
(setq linum-format "%4d ")

;; For JavaScript
(setq js-indent-level 4)
(setq js-switch-indent-offset 4)
(setq-default indent-tabs-mode nil)



;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ;; scroll window under mouse
(setq scroll-step 1)                                ;; keyboard scroll one line at a time


;;no extra whitespace after lines
(add-hook 'before-save-hook 'whitespace-cleanup)


;; HTML indenting
(add-hook 'html-mode-hook
    (lambda ()
        ;; Default indentation is usually 2 spaces, changing to 4.
        (set (make-local-variable 'sgml-basic-offset) 4)))


;; ############################################################################
;;                          Setup Shortcuts
;; ############################################################################

;; Show "buffer" with "cuts"
(global-set-key "\M-y" 'browse-kill-ring)

;; Move line or block up/down
(global-set-key "\M-n" 'move-text-down)
(global-set-key "\M-p" 'move-text-up)

;; Toggle SrSpeebar
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

;; For Wind Move
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


;; ###########################################################################
;;                        Color Theme Stuff
;; ###########################################################################

;; Before you must to clone theme in folder ~/.emacs.d/themes
;; git clone https://github.com/sellout/emacs-color-theme-solarized.git

;; current color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)


;; Add syntax highlighting for operators ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Currently support for []|&!.+=-/%*,()<>{}
(font-lock-add-keywords
 'python-mode '(("\\(\\[\\|\\]\\|[|!\\.\\+\\=\\&]\\|-\\|\\/\\|\\:\\|\\%\\|\\*\\|,\\|(\\|)\\|>\\ |<\\|{\\|}\\)" 1 font-lock-operator-face )
                ("\\(;\\)" 1 font-lock-end-statement )))

(make-face 'font-lock-operator-face)
(make-face 'font-lock-end-statement)
(setq font-lock-operator-face 'font-lock-operator-face)
(setq font-lock-end-statement 'font-lock-end-statement)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


;; Setup my custom colors, but Emacs must be run through
;; color preset "Solarized Dark" in iTerm2
;; Color names got from this site:  https://jonasjacek.github.io/colors/
(custom-set-faces
 '(font-lock-function-name-face ((t (:foreground "Gold1"))))
 '(font-lock-keyword-face ((t (:foreground "SteelBlue1"))))
 '(font-lock-string-face ((t (:foreground "#00af5f"))))
 '(font-lock-type-face ((t (:foreground "Orange1" :bold t))))
 '(font-lock-builtin-face ((t (:foreground "SlateBlue3"))))
 '(font-lock-variable-name-face ((t (:foreground "#949494"))))
 '(font-lock-comment-face ((t (:foreground "#559fff"))))
 '(font-lock-operator-face ((t (:foreground "#d7af00"))))
 '(py-decorators-face ((t (:foreground "#ad7fa8" :bold t))))
 )

;;; init.el ends here
