;; Emacs Configuration
;; Eric Zhiqiang Ma, http://www.ericzma.com
;; Check more tips at:
;; http://www.fclose.com/4249/emacs-tips-and-howtos/
;; and
;; http://ask.fclose.com/tag/emacs

;; Modes not tracked by git here and you may need to 
;; install them manually:
;; Auto Complete Mode: http://www.fclose.com/4249/emacs-tips-and-howtos/#auto-completion

;; ===============common config==================

;; load path
(add-to-list 'load-path "~/.emacs.lisp/")

;; recursively load subdirs in ~/.emacs.lisp/
(let ((default-directory "~/.emacs.lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; interface
;; http://www.emacswiki.org/emacs/ColorTheme
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-robin-hood)
;; (color-theme-charcoal-black)
;; (color-theme-gray30)

;; use the new deftheme support from Emacs 24
(load-theme 'misterioso t)

;; font size
;; (set-face-attribute 'default nil :height 120)

;; No start up message
(setq inhibit-startup-message t)

;; No beep warning
(setq visible-bell t)

;; set foreground color
;;(set-foreground-color "white")

;; set background color
;;(set-background-color "gray14")

;; No scroll bar
;; (scroll-bar-mode nil)

;; No tool bar
;; (tool-bar-mode -1)

;; No menu bar
(menu-bar-mode -1)

;; Set the frame size
;; (defun set-frame-size()
;;   (interactive)
;;   (if window-system
;;   (progn
;;      (setq initial-frame-alist '((width . 140) (height . 42))))))
;; 
;; (set-frame-size)

;; Display line and column number at the status bar
(setq column-number-mode t)
(setq line-number-mode t)

;; Display line number on the left of the content
;; http://www.logic.at/prolog/linum/linum.html
(require 'linum)
(global-linum-mode t)
;; use customized linum-format: add a addition space after the line number
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; Display the paren
;; see matching pairs of parentheses and other characters
(show-paren-mode t)

;; Only file name in the title bar
(setq frame-title-format (concat "%b"))

;; Replace tab with spaces
(setq-default indent-tabs-mode nil)

;; Set default tab width
(setq-default tab-width 4)

;; Auto save mode
(setq auto-save-mode nil)

;; Set backup functions
(setq
  backup-by-copying t ; auto backup
  backup-directory-alist '(("."."~/.emacs.d/bak")) ; store backup files in "~/.emacs.d/bak"
  version-control t ; backup version control
  delete-old-versions t ; automatically delete old backup files
  kept-new-versions 6 ; keep only 6 version of backup
  kept-old-versions 2 ; keep the 2 oldest version of backup
)

;; Set default major mode to text-mode
(setq default-major-mode 'text-mode)

;; keys
(global-set-key "\C-c\C-x" 'kill-whole-line)

;; automatically start server
;; (server-start)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; ===================end common config=============

;; =================== etags =====================
(defvar tags-cmd "etags -R ./* 2>/dev/null")

(defun regen-tags ()
  "Regenerate the tags file for the current working directory"
  (interactive)
  (let ((tag-file (concat default-directory "TAGS")))
    (shell-command tags-cmd)
    (visit-tags-table tag-file)))
;; =================== end etags =====================

;; ==================c/c++====================
(add-hook 'c-mode-hook 'linux-c-mode)

(defun linux-c-mode()
  (interactive)
  ;; Set the alias style of C
  (c-set-style "K&R")
  ;; Auto align after typed {
  (c-toggle-auto-state)
  ;; Hungry delete spaces when pressing Backspace
  (c-toggle-hungry-state)
  ;; Set width of TAB to 4
  (setq c-basic-offset 4)
  ;; Display the function name
  (which-function-mode)
  ;; Key define - compile
  (define-key c-mode-base-map [(f7)] 'compile)
  ;; Enter act same as C-j
  (define-key c-mode-map [return] 'newline-and-indent)
  ;; Code auto completion
  (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
)

(add-hook 'c++-mode-hook 'linux-c++-mode)

(defun linux-c++-mode()
  (interactive)
  (c-set-style "K&R")
  (c-toggle-auto-state)
  (c-toggle-hungry-state)
  (setq c-basic-offset 4)
  (imenu-add-menubar-index)
  (which-function-mode)
  (define-key c-mode-base-map [(f7)] 'compile)
  (define-key c++-mode-map [return] 'newline-and-indent)
  (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
)

;; complete inside a word, otherwise indent
(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
    (hippie-expand nil)
    (indent-for-tab-command)))

(autoload 'senator-try-expand-semantic "senator")

;; hippie auto expand, use senator first
(setq hippie-expand-try-functions-list
 	  '(
		senator-try-expand-semantic
		try-expand-dabbrev
		try-expand-dabbrev-visible
		try-expand-dabbrev-all-buffers
		try-expand-dabbrev-from-kill
		try-expand-list
		try-expand-list-all-buffers
		try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        )
)

;; define the make command
'(compile-command "make")

;; ==================end c/c++================

;; ==================tuareg===================
;; tuareg-mode set
(setq auto-mode-alist (cons '(".mlw?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; ==================end tuareg===============

;; ==================scala===============
;; scala mode
(require 'scala-mode2)

;; sbt mode
(require 'sbt-mode)

;; Load the ensime lisp code...
;; (add-to-list 'load-path "ENSIME_ROOT/elisp/")
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; auto indent: http://www.emacswiki.org/emacs/AutoIndentation
(add-hook 'scala-mode-hook '(lambda ()
                              (local-set-key (kbd "RET") 'newline-and-indent)))

;; ==================end scala===============

;; ==================latex===================
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; ==================end latex===================
