;; You should already installed:
;;    - pymacs
;;    - ropemacs
;;    - color-theme

;; Path to ma libs
(add-to-list 'load-path "~/.emacs.d/")


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


;; For run use 'M-x pylint'
(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")


;; Navigate alt + 'arrow keys'
(windmove-default-keybindings 'meta)


;; Hide toolbar
(tool-bar-mode -1)


;; Hide scroll bar
 (scroll-bar-mode -1)


;; Line numbers
(global-linum-mode 1)


;; Show the column and line numbers
(setq line-number-mode t)
(setq column-number-mode t)


;; Show the right border (M-x fci-mode)
(require 'fill-column-indicator)
(setq fci-handle-line-move-visual nil)
(setq-default fill-column 79)
(setq fci-style 'rule)


;; Line by line scrolling
(setq scroll-step 1)


;; Cursor type
(defun set-cursor ()
  (setq cursor-in-non-selected-windows nil)
  (setq cursor-type '(bar . 1)))
(add-hook 'post-command-hook 'set-cursor)


;; Do not show a message at startup
(setq inhibit-startup-message t)


;; 'Python-style' tabs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)


;; Setup the basic font Consolas
(set-face-font 'default "-microsoft-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")


;; Load a theme from a file
(autoload 'color-theme-orico-black "color-theme-orico-black")

(require 'color-theme) ;; loads "coloring module"
(color-theme-initialize) ;; load color schemes
(color-theme-orico-black) ;; select a particular scheme



;; Now I'm not using flymake =================================================
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



;; Duplicates the current line or region =====================================
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
;; ===========================================================================



(put 'upcase-region 'disabled nil)

