;; Emacs Configuration
;; Eric Zhiqiang Ma, http://www.ericzma.com

;; Check more tips at:
;; http://www.fclose.com/4249/emacs-tips-and-howtos/
;; and
;; http://ask.fclose.com/tag/emacs

;; load path
(add-to-list 'load-path "~/.emacs.lisp/")

;; recursively load subdirs in ~/.emacs.lisp/
(let ((default-directory "~/.emacs.lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; package repos
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; activate installed packages
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
  Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       ;; (if (y-or-n-p (format "Package %s is missing. Install it? " package))
       (package-install package)
       ;;  package)
       ))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))


(ensure-package-installed
 'evil
 'undo-tree
 'linum
 'fill-column-indicator
 'auto-complete
 'smex
 'flyspell
 'reftex
 'scala-mode2
 'sbt-mode
 'ensime) ; --> (nil ...) if packages are not already installed

;; ================= common config =============
;; Set default major mode to text-mode
(setq default-major-mode 'text-mode)

;; automatically start server
;; (server-start)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; ================= end common config =============

;; =============== auto-actions ==================
;; Auto save mode
(setq auto-save-mode nil)

;; Set backup functions
(setq
  backup-by-copying t ; auto backup
  backup-directory-alist '(("."."~/.emacs.d/backup")) ; backup files
  auto-save-file-name-transforms `((".*" ,"~/.emacs.d/backup" t)) ; #files
  version-control t ; backup version control
  delete-old-versions t ; automatically delete old backup files
  kept-new-versions 8 ; keep 8 version of backup
  kept-old-versions 4 ; keep the 4 oldest version of backup
)

;; Auto Complete Mode: http://www.fclose.com/4249/emacs-tips-and-howtos/#auto-completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; automatically delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; automatic spell checking
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;; =============== end auto-actions ==================

;; =============== interface ==================

;; use the new deftheme support from Emacs 24
(if (>= emacs-major-version 24)
    (load-theme 'misterioso t))

;; No start up message
(setq inhibit-startup-message t)

;; No beep warning
(setq visible-bell t)

;; font size
;; (set-face-attribute 'default nil :height 120)

;; set foreground color
;;(set-foreground-color "white")

;; set background color
;;(set-background-color "gray14")

;; No scroll bar
;; (scroll-bar-mode nil)

;; No tool bar
;; (tool-bar-mode -1)

;; Set the frame size
;; (defun set-frame-size()
;;   (interactive)
;;   (if window-system
;;   (progn
;;      (setq initial-frame-alist '((width . 140) (height . 42))))))
;;
;; (set-frame-size)

;; No menu bar
(menu-bar-mode -1)

;; Display line and column number at the status bar
(setq column-number-mode t)
(setq line-number-mode t)

;; Only file name in the title bar
(setq frame-title-format "%b (%f)")

;; buffer name with the path
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Display line number on the left of the content
;; http://www.logic.at/prolog/linum/linum.html
(require 'linum)
(global-linum-mode t)
;; use customized linum-format: add a addition space after the line number
(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; Display the paren
;; see matching pairs of parentheses and other characters
(show-paren-mode t)

;; Replace tab with spaces
(setq-default indent-tabs-mode nil)

;; Set default tab width
(setq-default tab-width 4)

;; display trailing whitespace
(setq-default show-trailing-whitespace t)

;; `lines-tail`, highlight the part that goes beyond the limit of `whitespace-line-column`
;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)

;; =============== end interface ==================


;; =============== keys ==================
;; kill whole line
(global-set-key "\C-c\C-x" 'kill-whole-line)

;; copy whole line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key "\C-c\C-k" 'copy-line)

;; make window larger or smaller
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)

;; evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; auto-completion when typing commands
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; =============== end keys ==================

;; =================== tags =====================
(defvar tags-cmd "ctags -e -R ./* 2>/dev/null")

(defun tags-regen ()
  "Regenerate the tags file for the current working directory"
  (interactive)
  (let ((tag-file (concat default-directory "TAGS")))
    (shell-command tags-cmd)
    (visit-tags-table tag-file)))
;; =================== end etags =====================

;; ================== c/c++ ====================
(add-to-list 'auto-mode-alist '("\\.c0\\'" . c-mode))

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
  ;; (define-key c-mode-map [return] 'newline-and-indent)
  (local-set-key (kbd "RET") 'newline-and-indent)
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
  ;; (define-key c++-mode-map [return] 'newline-and-indent)
  (local-set-key (kbd "RET") 'newline-and-indent)
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

;; ================== end c/c++ ================

;; ================== ocaml ===================
;; tuareg-mode set
(setq auto-mode-alist (cons '(".mlw?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
;; ================== end ocaml ===============

;; ================== scala ===============
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

;; set evil-mode M-. to be handled by ensime
(add-hook 'ensime-mode-hook '(lambda ()
                               (define-key evil-normal-state-local-map "\M-\." 'ensime-edit-definition)))

;; ================== end scala ===============

;; ================== text ===================
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; ================== end text ===================

;; ================== latex ===================
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; spell checking
(add-hook 'LaTex-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'latex-mode-hook (lambda () (flyspell-mode 1)))
;; ================== end latex ===================
