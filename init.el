;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-
;; Copyright (c) 2011-2022, Nick Blokhin
;; All rights reserved.

;;; Commentary:
;; Do not forget to feed cats.

;;; Code:

;; General stuff for packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

;; list the packages you want
(setq package-list
      '(web-mode tangotango-theme))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(web-mode tangotango-theme nord-theme company-lsp lsp-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; lsp mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'lsp-mode)

(use-package lsp-mode
  :commands lsp)

;; for completions
(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends))


;; web-mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))


;; Line numbers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Cursor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(blink-cursor-mode 2)
(setq-default cursor-type 'bar)
(set-cursor-color "#339933") 


;; Auto save and backups ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq make-backup-files nil)          ;; don't make any backup files
(setq auto-save-default nil)          ;; don't make aout saving
(setq auto-save-list-file-name nil)   ;; don't make any .saves files


;; General settings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(global-set-key (kbd "<escape>")
				'keyboard-escape-quit)        ;; Make ESC quit prompts

(set-face-attribute 'default nil
					:family "JetBrains Mono"
					:height 140)              ;; Setup font

(setq inhibit-startup-message t)              ;; Turn off startup messages
(setq visible-bell nil)                       ;; Set up the visible bell
(setq-default indent-tabs-mode t)             ;; Use tabs for indentation
(setq-default tab-width 4)                    ;; Set default tab width
(setq-default truncate-lines t)               ;; Don't wrap lines
(fset 'yes-or-no-p 'y-or-n-p)                 ;; y/n for  answering yes/no questions
(delete-selection-mode t)                     ;; Delete the selection with a keypress
(global-auto-revert-mode t)                   ;; Update buffers when files are changed externally
(setq history-delete-duplicates t)            ;; Delete duplicates in history


(global-hl-line-mode t)                       ;; Highlight line with cursor
(set-face-background 'hl-line "#3e4446")      ;; Highlight color of line with cursor


(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)          ; Disable visible scrollbar
      (tool-bar-mode -1)            ; Disable the toolbar
      (menu-bar-mode -1)))          ; Disable the menu bar


(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)


(load-theme 'tangotango t)
;; (load-theme 'nord t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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



;;; init.el ends here
