;; -*- coding: iso-8859-1 -*-
;;
;; Emacs initialization file.
;; (c) Semih Cemiloglu
;; $Id$
;; 

(message "INIT: Starting...")

(setq emacs-load-start-time (current-time))

(defconst win32p
    (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst cygwinp
    (eq system-type 'cygwin)
  "Are we running on a WinTel cygwin system?")

(defconst linuxp
    (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst solarisp
    (or (eq system-type 'Solaris)
        (eq system-type 'SunOS))
  "Are we running on a Solaris system?")

(defconst unixp
  (or linuxp
      (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix))
  "Are we running Unix")

(defconst linux-x-p
    (and window-system linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst xemacsp (featurep 'xemacs)
  "Are we running XEmacs?")

(defconst emacs>=21p (and (not xemacsp) (or (= emacs-major-version 21) (= emacs-major-version 22) (= emacs-major-version 23)))
  "Are we running GNU Emacs 21 or above?")

(defvar emacs-debug-loading nil)


;; Customizations changes made by Emacs go to this file
;; It may be useful to load this up as the first thing in intialization.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;-----------------------------------------------------
;; Defaults
;;-----------------------------------------------------

(cua-mode 0)

;; Background color
(setq default-frame-alist
      (append default-frame-alist
       '((foreground-color . "Black")
 (background-color . "Gray93")
 (cursor-color . "Blue")
 (cursor-type . 'hollow)
 )))

;; Cursor related 
(blink-cursor-mode nil) ; 1 to blink it on
(setq x-stretch-cursor t) ; stretch cursor to cover glyph

;; Don't use Win32 I-beam cursor 
(if win32p
    (setq w32-use-visible-system-caret  nil))

;; Horizontal line at cursor location
(global-hl-line-mode 1)
(set-face-background 'hl-line "White")

;; Smooth scrolling
(setq scroll-conservatively 1)

;; Position of the scroll bar
(set-scroll-bar-mode 'right)

;; Recent files
(recentf-mode 1)

;; Highlighted Region
(transient-mark-mode 1)

;; Make delete or typing delete whole selected text
(delete-selection-mode 1)

;; Where am I?
(line-number-mode 1)
(column-number-mode 1)

;; No yes-no question
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't convert leading spaces to tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Make a file executable if it's a script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Soft wrapping at the word boundary
(global-visual-line-mode 0) ; 1 for on, 0 for off.

;; Each file in a new window
;(setq pop-up-frames t)

; turn on highlight matching parens when cursor is on one
(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ; highlight just parens
;(setq show-paren-style 'expression) ; highlight entire expression

;; Default font
(if win32p
    (set-face-font 'default "Courier New-11")
    (set-face-font 'default "Monospace-10"))

;; Uncomment this to stop and debug on error
;(setq debug-on-error t) 

;; See Manual section 8.1.2
;; If the variable kill-whole-line is non-nil, 
;; C-k at the very beginning of a line kills the 
;; entire line including the following newline. This variable is normally nil. 
(setq kill-whole-line 1)


;; Don't pass Windows/Menu keystokes to OS.
;; setting the PC keyboard's various keys to Super or Hyper
;; presumes you are using the emacsw32 distro on windows
;; http://ourcomments.org/Emacs/EmacsW32Util.html
(setq w32-pass-lwindow-to-system nil 
      w32-pass-rwindow-to-system nil 
      w32-pass-apps-to-system nil 
      w32-lwindow-modifier 'super ;; Left Windows key (super)
      w32-rwindow-modifier 'super ;; Right Windows key (super)
      w32-apps-modifier 'hyper)   ;; Menu key (hyper)


;; X Windows Hyper key settings to capture hyper key events
(setq x-hyper-keysym 'hyper)


;; Allow coloring in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Enable auto reverting
;; See "Reverting a buffer" section of the manual
;(setq global-auto-revert-mode t)

;; Enable filesets
(filesets-init)

;; Coding system setting
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;;-----------------------------------------------------
;; Packages bundled with Emacs
;;-----------------------------------------------------

;; Desktop
(desktop-save-mode 0) ; 1-on 0-off
;; Customization follows below
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)


;; Common Lisp compatibility
;(require 'cl)

;; Dired-X mode by default
(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

;; Do not sit at the beginning of compilation window
;(setq compilation-scroll-output t)
(setq compilation-scroll-output 'first-error)

;; Tempo template 
;; TODO: Decide if this is better than YaSnippet 
;; (require 'tempo)
;; (setq tempo-interactive t)

;; icomplete mode provides dynamic completion preview
(icomplete-mode 99)

;; Use iswitch-mode for easy buffer switching
(iswitchb-mode t)

;; Method for making buffers unique
(setq uniquify-buffer-name-style 'post-forward)

;; Enable EDE mode for all buffers
;(global-ede-mode t)

;; Hippie expand
(setq hippie-expand-try-functions-list 
      '(yas/hippie-try-expand
        try-expand-dabbrev 
        try-expand-dabbrev-all-buffers 
        try-expand-dabbrev-from-kill 
        try-complete-file-name-partially 
        try-complete-file-name 
        try-expand-all-abbrevs 
        try-expand-list 
        try-expand-line 
        try-complete-lisp-symbol-partially 
        try-complete-lisp-symbol))
; See hippie-expand key-mapping below

;;-----------------------------------------------------
;; For Qt development
;;-----------------------------------------------------

;; On Windows we need following directories on PATH
(if win32p
  (progn
   ; These directories should come before pre-exisiting paths
   (setenv "PATH" (concat "I:/Local/bin;"  (getenv "PATH")))
   (setenv "PATH" (concat "I:/TCenv;" (getenv "PATH")))
   ; These directories should come after pre-existing paths
   (setenv "PATH" (concat "I:/TCenv/ctags-5.8;"  (getenv "PATH")))
   ;; (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/Aspell/bin"))
   ;; (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/GnuWin32/bin"))
   ;; (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/UnxUtils/usr/local/wbin"))
   ;; (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/doxygen-1.6.1/bin"))
   ;; (setenv "PATH" (concat (getenv "PATH") ";C:/Racket"))
   ;; (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/STAF-3.3.5/bin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/Program Files/Debugging Tools for Windows (x86)")) ; For CDB debugger
   ))

;; Default shell
;; TODO: Decide what to do with these
;; (if win32p
;;   (setq explicit-shell-file-name "cmd.exe")
;;   (setq explicit-shell-file-name "/bin/bash"))

;; Qt project files should be viewed in makefile-mode
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

;; On Win32 Default build command is MS nmake
(if win32p
  (setq compile-command "nmake")
  (setq compile-command "gmake"))


;;-----------------------------------------------------
;; Paths for third party packages, files, and others
;;-----------------------------------------------------

;; Keep all backup files in one directory 
(push '("." . "~/.emacs.backups") backup-directory-alist)

(add-to-list 'load-path
             "~/.emacs.d")

(add-to-list 'load-path
             "~/.emacs.lisp/misc")

;; (add-to-list 'load-path
;;              "~/.emacs.lisp/drews")



;;-----------------------------------------------------
;; Third party packages
;;-----------------------------------------------------

;; redo.el
;;
;; Redo/Undo system
(require 'redo)


;; cycle-buffer.el
;;
;; To select buffers.
(autoload 'cycle-buffer 
           "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward 
          "cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive 
          "cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive 
          "cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting 
          "cycle-buffer" "Toggle if this buffer will be considered." t)
; See key assignments at the end of this file


;; session.el
;;
;; Restores various variables from your last session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)


;; cursor-chg.el
;;
;; Change cursor dynamically, depending on the context
(require 'cursor-chg)  ; Load this library
(change-cursor-mode 1) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle
; Default values
(setq curchg-default-cursor-type 'hollow)
(setq curchg-idle-cursor-type 'box)


;; pager.el
;;
;; Window scroll commands
(require 'pager)
; See key assignments at the end of this file


;; maxframe.el
;;
;; Maximize/minimize/restore frames
(require 'maxframe)
;(add-hook 'window-setup-hook 'maximize-frame t) 


;; Google's C/C++ style for c-mode
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; For Scheme development
;(require 'quack)        

;; For Cobol development
(require 'cobol-mode)
(add-to-list 'auto-mode-alist '("\\.cob\\'" . cobol-mode))
(add-to-list 'auto-mode-alist '("\\.pco\\'" . cobol-mode))
(add-to-list 'auto-mode-alist '("\\.lst\\'" . cobol-mode))

;; For cdb debugger use
(load "cdb-gud")

;;-----------------------------------------------------
;; YaSnippet
;;-----------------------------------------------------

(add-to-list 'load-path
             "~/.emacs.lisp/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.lisp/yasnippet-0.6.1c/snippets")


;;-----------------------------------------------------
;; Org Mode
;;-----------------------------------------------------

(add-to-list 'load-path
             "~/.emacs.lisp/org-7.4/lisp")
(add-to-list 'load-path
             "~/.emacs.lisp/org-7.4/contrib/lisp")

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only

(setq org-link-abbrev-alist
  '(("bugzilla" . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
    ("google" . "http://www.google.com/search?q=")
    ("gmap" . "http://maps.google.com/maps?q=%s")
    ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
    ("ads" . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")))

(setq org-log-done t)

;;-----------------------------------------------------
;; Dired - Windows integration
;;-----------------------------------------------------

;; (defun open-externally (filename)
;;   (shell-command (concat "start " (shell-quote-argument filename))))
(defun open-externally (filename)
  (w32-shell-execute nil filename))

(defun is-file-type? (filename type)
  (string= type (substring filename (- (length filename) (length type)))))

(defun should-open-externally? (filename)
  (let ((file-types '(".pdf" ".doc" ".xls" ".docx" ".xlsx")))
    (member t (mapcar #'(lambda (type) (is-file-type? filename type)) file-types))))

(defadvice find-file (around find-file-external-file-advice (filename &optional wildcards))
  "Open non-emacs files with an appropriate external program"
  (if (should-open-externally? filename)
      (open-externally filename)
    ad-do-it))

(ad-activate 'find-file)


;;-----------------------------------------------------
;; Slime - ECL
;;-----------------------------------------------------

(setq inferior-lisp-program "I:/TCenv/ecl-11.1.1/ecl.exe") ; Lisp system

;; (add-to-list 'load-path "~/.emacs.lisp/slime-20110227")  ; SLIME directory
;; (require 'slime)
;; (slime-setup)


;;-----------------------------------------------------
;; Functions
;;-----------------------------------------------------

(defun text-scale-normal-size ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

;;
;; From ErgoEmacs
;;

(defun kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n")
      (delete-char -1)
    (kill-line 0)
    )
  )

(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1)
  )

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1)
  )

(defun switch-to-next-frame ()
  "Select the next frame on current display, and raise it."
  (interactive)
  (other-frame 1)
  )

(defun switch-to-previous-frame ()
  "Select the previous frame on current display, and raise it."
  (interactive)
  (other-frame -1)
  )


(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.


;;-----------------------------------------------------
;; Keyboard shortcuts:
;;-----------------------------------------------------

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; TODO: These are probably not needed
;; (global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "M-z") 'redo)

(global-set-key (kbd "M-/") 'hippie-expand)

;; Ergonomic key positioning
;; 
(global-set-key (kbd "M-j") 'backward-char) 
(global-set-key (kbd "M-k") 'next-line) 
(global-set-key (kbd "M-l") 'forward-char) 

(global-set-key (kbd "M-u") 'backward-word)  
(global-set-key (kbd "M-i") 'previous-line) 
(global-set-key (kbd "M-o") 'forward-word) 

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)




;; From pager.el
(global-set-key (kbd "<prior>")  'pager-page-up)   ; PageUp
(global-set-key (kbd "<next>")   'pager-page-down) ; PageDown
; Following two collides with CUA mode
(global-set-key [(control v)]  'pager-page-down) 
(global-set-key [(meta v)]     'pager-page-up) 
(global-set-key [(meta up)]    'pager-row-up) 
(global-set-key [(meta kp-8)]  'pager-row-up) 
(global-set-key [(meta down)]  'pager-row-down) 
(global-set-key [(meta kp-2)]  'pager-row-down)


;; H is for hyper (Menu key)
;; s is for super (Windows key)
(global-set-key (kbd "s-c") 'make-frame-command) 
(global-set-key (kbd "s-x") 'delete-frame)
(global-set-key (kbd "H-d") 'delete-frame)
(global-set-key (kbd "H-x") 'maximize-frame)
(global-set-key (kbd "H-n") 'w32-minimize-frame)
(global-set-key (kbd "H-r") 'restore-frame)
(global-set-key (kbd "s-k") 'describe-key)
(global-set-key (kbd "H-f") 'describe-function)
(global-set-key (kbd "s-<delete>") 'kill-buffer)

; Frame movement
(global-set-key (kbd "s-n") 'switch-to-next-frame)
(global-set-key (kbd "s-<right>") 'switch-to-next-frame)
(global-set-key (kbd "s-<left>")  'switch-to-previous-frame)
(global-set-key (kbd "C-<tab>")   'switch-to-next-frame)
(global-set-key (kbd "C-S-<tab>") 'switch-to-previous-frame)

; Buffer movement
(global-set-key (kbd "M-<right>") 'cycle-buffer)
(global-set-key (kbd "M-<left>")  'cycle-buffer-backward)
(global-set-key (kbd "M-S-<right>") 'cycle-buffer-permissive)
(global-set-key (kbd "M-S-<left>")  'cycle-buffer-backward-permissive)

 
;; Font height
;(global-set-key (kbd "C-+") 'text-scale-increase)
;(global-set-key (kbd "C--") 'text-scale-decrease)
;(global-set-key (kbd "C-0") 'text-scale-normal-size)
(global-set-key (kbd "C-<kp-add>") 'text-scale-increase)
(global-set-key (kbd "C-<kp-subtract>") 'text-scale-decrease)
(global-set-key (kbd "C-<kp-0>") 'text-scale-normal-size)


;; Function key bindings
(global-set-key (kbd "<f5>") 'dired)
(global-set-key (kbd "<f6>") 'ibuffer)
(global-set-key (kbd "<f7>") 'recompile)
(global-set-key (kbd "<f8>") 'ff-find-other-file)


;;-----------------------------------------------------
;; File associations
;;-----------------------------------------------------

;; For xml files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; Well known file types for makefile-mode
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.pri\\'" . makefile-mode))

;; Python mode
(add-to-list 'auto-mode-alist '("\\.py$\\'" . python-mode))



;;-----------------------------------------------------
;; Check point
;;-----------------------------------------------------

(message "INIT: Completed.")

;; End of file
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
