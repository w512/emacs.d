;; You should already installed:
;;    - pymacs
;;    - ropemacs
;;    - emacs-goodies-el (for color-theme and tabbar)
;;    - ECB (http://ecb.sourceforge.net)
;;
;;
;; All hotkeys define in my config:
;;   'F1' or 'M-x pylint'    - run pylint
;;   'F2' or 'M-x fci-mode'  - show the right border
;;   'C-c c'                 - duplicates the current line or region
;;   'alt + [arrow keys]'    - frame navigation
;;   'C-x C-u'               - upcase region
;;   'C-x C-l'               - downcase region
;;   'C-c <C-left|right>'    - tabbar backward|forward


;; Path to my libs and scripts
(add-to-list 'load-path "~/.emacs.d/")


;; The guys from Canonical broken 'ecb' in Ubuntu 11.04
;; (require 'ecb)    ;; Emacs Code Browser


;; ecb alternative ('C-c s' - to run)
(require 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)


(custom-set-variables
 '(ecb-options-version "2.32")
 '(ecb-tip-of-the-day nil)
 ;; '(ecb-auto-activate t)
 '(tool-bar-mode nil)            ;; Hide toolbar
 '(scroll-bar-mode nil)          ;; Hide scroll bar
 '(global-linum-mode t)          ;; Show line numbers
 '(scroll-step t)                ;; Line by line scrolling
 '(inhibit-startup-message t)    ;; Do not show a message at startup
 '(require-final-newline t)      ;; Put new line in file end
 '(iswitchb-mode t)              ;; Cool buffers switching
)


;; Show tabs (backward - 'C-c <C-left>', forward - 'C-c <C-right>')
(require 'tabbar)
(tabbar-mode t)


;; Frame navigation alt + [arrow keys]
(windmove-default-keybindings 'meta)


;; Show the column and line numbers
(setq line-number-mode t)
(setq column-number-mode t)


;; Cursor type
(defun set-cursor ()
  (setq cursor-in-non-selected-windows nil)
  (setq cursor-type '(bar . 1)))
(add-hook 'post-command-hook 'set-cursor)


;; Setup the basic font Consolas
(set-face-font 'default "-microsoft-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")


;; Load a theme from a file
(autoload 'color-theme-orico-black "color-theme-orico-black")
(color-theme-orico-black) ;; and select this scheme


;; If you want try other themes
;; (require 'color-theme)     ;; loads "coloring module"
;; (color-theme-initialize)   ;; load color schemes



;; ===========================================================================
;; ===========================   Python Settings   ===========================
;; ===========================================================================

;; Initialize Python-mode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; Initialize Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)


;; Initialize Rope
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)


;; For run use 'M-x pylint' or 'F1'
(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")
(global-set-key [f1] 'pylint)


;; 'Python-style' tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)


;; Show the right border ('M-x fci-mode' or 'F2')
(require 'fill-column-indicator)
(setq-default fill-column 79)
(setq fci-style 'rule)
(setq-default fci-rule-character ?|)
(global-set-key [f2] 'fci-mode)
;; turn on this option for all new buffers using python-mode
;; (add-hook 'python-mode-hook 'fci-mode)


;; Now I'm not using flymake
;;(when (load "flymake" t)
;;  (defun flymake-pylint-init ()
;;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                       'flymake-create-temp-inplace))
;;           (local-file (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;      (list "epylint" (list local-file))))
;;
;;  (add-to-list 'flymake-allowed-file-name-masks
;;               '("\\.py\\'" flymake-pylint-init)))
;;
;;(add-hook 'python-mode-hook 'flymake-mode)

;; ===========================================================================






;; Duplicates the current line or region (C-c c)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c c") 'duplicate-current-line-or-region)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



(put 'upcase-region 'disabled nil)    ;; C-x C-u
(put 'downcase-region 'disabled nil)  ;; C-x C-l

