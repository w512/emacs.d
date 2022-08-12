;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-
;; Copyright (c) 2011-2022, Nick Blokhin
;; All rights reserved.

;;; Commentary:
;; Do not forget to feed cats.

;;; Code:


;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;
;; General stuff for packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; list the packages you are used
(setq package-list
      '(web-mode tangotango-theme doom-themes all-the-icons))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))


;;
;; web-mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))


;;
;; Line numbers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(global-display-line-numbers-mode t)
(column-number-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(global-display-fill-column-indicator-mode t)            ;; Show right border
(setq-default display-fill-column-indicator-column 119)  ;; Border position
(set-face-foreground 'fill-column-indicator "#004400")   ;; Border color

;;
;; Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(blink-cursor-mode 2)
(setq-default cursor-type 'bar)
(set-cursor-color "#339933") 

(global-hl-line-mode t)                      ;; Highlight line with cursor
(set-face-background 'hl-line "#3e4446")     ;; Highlight color of line with cursor


;;
;; Auto save and backups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;
(setq make-backup-files nil)          ;; don't make any backup files
(setq auto-save-default nil)          ;; don't make aout saving
(setq auto-save-list-file-name nil)   ;; don't make any .saves files


;;
;; General settings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;

(desktop-save-mode 1)                         ;; Save editor state between loads

(global-set-key (kbd "<escape>")
				'keyboard-escape-quit)        ;; Make ESC quit prompts

(set-face-attribute 'default nil
					:family "JetBrains Mono"
					:height 160)              ;; Setup font

(setq line-spacing 0.3)					      ;;  space to put between lines

(setq inhibit-startup-message t)              ;; Turn off startup messages
(setq visible-bell nil)                       ;; Set up the visible bell
(setq history-delete-duplicates t)            ;; Delete duplicates in history
(setq-default indent-tabs-mode t)             ;; Use tabs for indentation
(setq-default tab-width 4)                    ;; Set default tab width
(setq-default standart-indent 4)              ;; Standart indent size
(setq-default truncate-lines t)               ;; Don't wrap lines
(fset 'yes-or-no-p 'y-or-n-p)                 ;; y/n for  answering yes/no questions
(delete-selection-mode t)                     ;; Delete the selection with a keypress
(electric-pair-mode t)                        ;; Autoclose {},[],()
(global-auto-revert-mode t)                   ;; Update buffers when files are changed externally
(setq global-auto-revert-non-file-buffers t)  ;; Update Dired and other buffers on change

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)     ;; Disable visible scrollbar
      (tool-bar-mode -1)       ;; Disable the toolbar
      (menu-bar-mode -1)))     ;; Disable the menu bar


(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; (load-theme 'tangotango t)
;; (load-theme 'nord t)


(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ;; if nil, italics is universally disabled
  (load-theme 'doom-solarized-dark t)  ;; select current theme (another good theme is "doom-spacegrey")
  (doom-themes-visual-bell-config)     ;; Enable flashing mode-line on errors
  (doom-themes-neotree-config)         ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))            ;; Corrects (and improves) org-mode's native fontification.

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun duplicate-line()
  "Duplicate the current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(global-set-key [(control super up)]  'move-line-up)
(global-set-key [(control super down)]  'move-line-down)
(global-set-key [(super shift d)] 'duplicate-line)


;; For Wind Move ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




;;
;; Colors customisation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;

(set-face-foreground 'web-mode-keyword-face "#bbbf2a")
(set-face-foreground 'web-mode-html-tag-bracket-face "#879393")
(set-face-foreground 'web-mode-html-attr-value-face "#009933")
(set-face-foreground 'web-mode-javascript-string-face "#009933")


(defvar font-lock-punctuation-face		
  'font-lock-my-face
  "Face name to use for punctuation.")

(defface font-lock-punctuation-face
  '((t (:foreground "#9C640C")))
  "Font Lock mode face used to highlight punctuation."
  :group 'font-lock-faces)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(
           ("\\({\\|}\\|\\[\\|\\]\\|(\\|)\\)" 0 'font-lock-punctuation-face prepend)
           ("\\( < \\| > \\|/\\|\\.\\|@\\|\\,\\|\\*\\)" 0 'font-lock-punctuation-face prepend)
           ("\\(:\\|=\\|;\\|\\+\\| - \\|\\-=\\|\\+=\\|>=\\|<=\\|!=\\|!==\\|=>\\)" 0 'font-lock-punctuation-face prepend)
           ;("TODO:" 0 'font-lock-punctuation-face prepend)
           ;("{%.*?block.*?%}" 0 'sm/django-block-face prepend)
           ;<more lines here...>
           )'end))
      '(web-mode))



;;; init.el ends here
