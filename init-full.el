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
;; CUA mode
;;-----------------------------------------------------
(cua-mode t)

;; Remove M-v keybinding in cua mode
(define-key cua--cua-keys-keymap [(meta v)] 'nil)
;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;-----------------------------------------------------
;; Defaults
;;-----------------------------------------------------

;; Default major mode is Text
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Disable system beep
;;(setq visible-bell t)

;; Stop the bell
(defun eat-bell () nil)
(setq ring-bell-function 'eat-bell)


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

;; Line numbers
;; M-x linum-mode
(require 'linum)
;(global-linum-mode 1) ; Display line numbers in margin

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
(global-visual-line-mode 1) ; 1 for on, 0 for off.

;; Each file in a new window
;(setq pop-up-frames t)

; turn on highlight matching parens when cursor is on one
(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ; highlight just parens
;(setq show-paren-style 'expression) ; highlight entire expression

;; Default font
(if win32p
    (set-face-font 'default "Courier New-11"))

; For studlyCaps, use (c-subword-mode 1) in your mode setup: 
; makes some movement and text commands recognize case-change 
; as a word boundary.
(c-subword-mode 1)

;; Uncomment this to stop and debug on error
;(setq debug-on-error t) 



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

;;-----------------------------------------------------
;; Packages bundled with Emacs
;;-----------------------------------------------------

;; Desktop
(desktop-save-mode 0) ; 1-on 0-off
;; Customization follows below
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)


;; Common Lisp compatibility
(require 'cl)

;; Dired-X mode by default
(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

;; Get into server mode
;; It may require manually creating directory "~/emacs.d/server"
(require 'server)
(when (and (= emacs-major-version 23) (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.
(server-start)


;; Thing-at-point+ enhancements
(eval-after-load "thingatpt" '(require 'thingatpt+))


;; For xml files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; Do not sit at the beginning of compilation window
(setq compilation-scroll-output t)


;;-----------------------------------------------------
;; For Qt development
;;-----------------------------------------------------

;; On Windows we need following directories on PATH
(if win32p
  (progn
   ; These directories should come before pre-exisiting paths
   (setenv "PATH" (concat "C:/TCenv/ctags-5.8;"  (getenv "PATH")))
   (setenv "PATH" (concat "C:/TCenv/msys-1.0/bin;"  (getenv "PATH")))
   (setenv "PATH" (concat "C:/TCenv;" (getenv "PATH")))
   ; These directories should come after pre-existing paths
   (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/Aspell/bin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/GnuWin32/bin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/UnxUtils/usr/local/wbin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/doxygen-1.6.1/bin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/SparkBuild-1.0/i686_win32/bin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/TCenv/STAF-3.3.5/bin"))
   (setenv "PATH" (concat (getenv "PATH") ";C:/Program Files/Debugging Tools for Windows (x86)")) ; For CDB debugger
   ))

;; Default shell
;; On Win32 MYSYS bash could be used
(if win32p
  (setq explicit-shell-file-name "C:/TCenv/msys-1.0/bin/bash.exe")
  (setq explicit-shell-file-name "/bin/bash"))

;; Qt project files should be viewed in makefile-mode
(add-to-list 'auto-mode-alist '("\\.pro\\'" . makefile-mode))

;; On Win32 Default build command is qnmake batch script
(if win32p
  (setq compile-command "C:/TCenv/qnmake.bat")
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

(add-to-list 'load-path
             "~/.emacs.lisp/drews")





;;-----------------------------------------------------
;; Third party packages
;;-----------------------------------------------------

;; Definitions as described in the book
;; "Teach Yourself Emacs"
(require 'sams-lib)


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

; See key assignments at the end of tis file
;(global-set-key [(f9)]        'cycle-buffer-backward)
;(global-set-key [(f10)]       'cycle-buffer)
;(global-set-key [(shift f9)]  'cycle-buffer-backward-permissive)
;(global-set-key [(shift f10)] 'cycle-buffer-permissive)
;(global-set-key [(meta right)]    'cycle-buffer) 
;(global-set-key [(meta left)]     'cycle-buffer-backward)


;; session.el
;;
;; Restores various variables from your last session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)


;; dot-mode.el
;;
;; Minor mode to repeat typing and commands 
(autoload 'dot-mode "dot-mode" nil t) ; vi `.' command emulation
(global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
                               (message "Dot mode activated.")))

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
(global-set-key (kbd "<prior>")  'pager-page-up)   ; PageUp
(global-set-key (kbd "<next>")   'pager-page-down) ; PageDown
; Following two collides with CUA mode
;(global-set-key [(control v)]  'pager-page-down) 
;(global-set-key [(meta v)]     'pager-page-up) 
(global-set-key [(meta up)]    'pager-row-up) 
(global-set-key [(meta kp-8)]  'pager-row-up) 
(global-set-key [(meta down)]  'pager-row-down) 
(global-set-key [(meta kp-2)]  'pager-row-down)


;; maxframe.el
;;
;; Maximize/minimize/restore frames
(require 'maxframe)
;(add-hook 'window-setup-hook 'maximize-frame t) 


;; dim-google.el
;;
;; Do Google search
(require 'dim-google)

;; Spell Checking (use the one bundled with Emacs)
(if win32p
  (setq-default ispell-program-name "C:/TCenv/Aspell/bin/aspell.exe")
  (setq-default ispell-program-name "spell"))

(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-process-directory (expand-file-name "~/"))

;; flyspell.el
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t) 
; the default flyspell behaviour
(put 'LeTex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)
; some extra flyspell delayed command
(mapcar 'flyspell-delay-command	'(scroll-up1 scroll-down1))


;; Enhancements to imenu.el
;(require 'imenu+)


;; Google's C/C++ style for c-mode
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; Enhancements to compile.el
(require 'compile+)

;; mode-compile.el
;;
;; To compile current buffer file based on the major mode
(autoload 'mode-compile "mode-compile" 
          "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
          "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)


;; mk-project.el
;;
;; To quickly switch between projects and 
;; perform operations on a per-project basis
(require 'mk-project)
(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file) ; or project-find-file-ido
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)


;; cdb-gud.el
;;
;; Support for Microsoft CDB (Command-line Debugger)
(if win32p
    (load "cdb-gud"))


;; autopair.el
;;
;; Making braces/quates automatically paired.

;(require 'autopair)

;(autopair-global-mode) ;; enable autopair in all buffers 
; use autopair just in c buffers 
;(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode))) 

; Ensure closing char is on separete line
(defun autopair-close-block (arg)
  (interactive "P")
  (cond
   (mark-active
    (autopair-close arg))
   ((not (looking-back "^[[:space:]]*"))
    (newline-and-indent)
    (autopair-close arg))
   (t
    (autopair-close arg))))

;(add-hook 'c-mode-common-hook
;          '(lambda ()
;             (local-set-key "(" 'autopair-insert)
;             (local-set-key ")" 'autopair-insert)
;             (local-set-key "{" 'autopair-insert)
;             (local-set-key "}" 'autopair-close-block)))


;; paredit.el
;;
;; Parenthesis-Editing Minor Mode

(autoload 'paredit-mode "paredit" 
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

; Enable it in following modes
; Warning: Collision with ErgoEmacs
;(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;; Use ElDoc with ParEdit
(require 'eldoc) ; if not already loaded
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)


;; defshell.el
;;
;; Run different inferior shells

(require 'defshell)


;; p4.el
;;
;; Perforce integration
;(load-library "p4")


;; command-frequency.el
;;
;; Record and tally Emacs commands, keystrokes
;(require 'command-frequency)
;(command-frequency-table-load)
;(command-frequency-mode 1)
;(command-frequency-autosave-mode 1)


;; python-mode.el
;;
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)


;;-----------------------------------------------------
;; Drew Andrews' Packages
;;-----------------------------------------------------

;(require 'doremi)

;(require 'lacarte)
; ESC M-x
;(global-set-key [?\e ?\M-x] 'lacarte-execute-menu-command)

;(require 'facemenu+)

; This will slow down font locking
;(require 'font-lock+)

; M-x palette
;(require 'palette) 


;;-----------------------------------------------------
;; Yasnippet Package
;;-----------------------------------------------------

(add-to-list 'load-path
             "~/.emacs.lisp/yasnippet-0.6.1c")
(require 'yasnippet) 
(yas/initialize)
(yas/load-directory "~/.emacs.lisp/yasnippet-0.6.1c/snippets")


;;-----------------------------------------------------
;; Slime Package
;;-----------------------------------------------------

; Set the inferior Lisp executable
(if win32p
  (setq inferior-lisp-program "C:/TCenv/ecl-9.12.3/ecl.exe")
  ;(setq inferior-lisp-program "C:/Program Files/Steel Bank Common Lisp/1.0.29/sbcl.exe")
  (setq inferior-lisp-program "/stow/ecl-9.10.3/bin/ecl"))

; Set the Slime paths
(add-to-list 'load-path "~/.emacs.lisp/slime-20091130") 
(add-to-list 'load-path "~/.emacs.lisp/slime-20091130/contrib")

; To avoid some start-up errors
(setq byte-compile-dynamic nil)

(require 'slime)
;(slime-setup)
;(slime-setup '(slime-repl))
(slime-setup '(slime-fancy))

;;-----------------------------------------------------
;; Icicles Package
;;-----------------------------------------------------

;(add-to-list 'load-path
;             "~/.emacs.lisp/icicles-20091127")

;(require 'icicles)
;(icy-mode 1)


;;-----------------------------------------------------
;; Org Mode
;;-----------------------------------------------------

(add-to-list 'load-path
             "~/.emacs.lisp/org-6.33f/lisp")
(add-to-list 'load-path
             "~/.emacs.lisp/org-6.33f/contrib/lisp")

(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only

(setq org-log-done t)


;;-----------------------------------------------------
;; ErgoEmacs section, should come after other packages
;;-----------------------------------------------------

;; ErgoEmacs keyboard layout
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us") ; US layout
;(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "colemak") ; Colemak layout
 
;; load ErgoEmacs keybinding
(load "~/.emacs.lisp/ergoemacs-keybindings-5.1/ergoemacs-mode")

;; hooks to disable confusing shortcusts
(add-hook 'ergoemacs-mode-hook 
 (lambda () 
  (progn
   (define-key ergoemacs-keymap (kbd "C-p") nil)
   (define-key ergoemacs-keymap (kbd "C-a") nil))))


;; turn on minor mode ergoemacs-mode
(ergoemacs-mode 0)


;;-----------------------------------------------------
;; Functions
;;-----------------------------------------------------

(defun text-scale-normal-size ()
  "Set the height of the default face in the current buffer to its default value."
  (interactive)
  (text-scale-increase 0))

(defun lookup-word-definition ()
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
 (interactive)
 (let (myword myurl)
   (setq myword
         (if (and transient-mark-mode mark-active)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myword (replace-regexp-in-string " " "%20" myword))
  (setq myurl (concat "http://www.answers.com/main/ntquery?s=" myword))

  (browse-url myurl)
  ;; (w3m-browse-url myurl) ;; if you want to browse using w3m

  ;; if you want to browse using OSX+Opera
  ;; (shell-command (concat "open -a opera " myurl))
))


(defun lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
This command generates a url for Wikipedia.com and switches you
to browser. If a region is active (a phrase), lookup that phrase."
 (interactive)
 (let (myword myurl)
   (setq myword
         (if (and transient-mark-mode mark-active)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (thing-at-point 'symbol)))

  (setq myword (replace-regexp-in-string " " "_" myword))
  (setq myurl (concat "http://en.wikipedia.org/wiki/" myword))
  (browse-url myurl)
))


(defun run-ctags ()
  "Run ctags on the current buffer's directory"
  (interactive)
  (save-excursion
    (let ((cb (current-buffer)))
      (set-buffer (get-buffer-create "*ctags*"))
      (call-process "C:/TCenv/ctags-5.8/ctags.exe" nil t nil "-V" "-e" "-R" "--languages=C,C++" "--extra=+q" ".")
      ;; Perform error checking here
      (goto-char (point-min))
      (if (looking-at "Error")
          (error (buffer-string)))
      )))


(defun run-doxygen ()
  "Run doxygen on the current buffer's directory"
  (interactive)
  (save-excursion
    (let ((cb (current-buffer)))
      (set-buffer (get-buffer-create "*doxygen*"))
      (call-process "C:/TCenv/doxygen-1.6.1/bin/doxygen.exe" nil t nil "doxy.cfg")
      ;; Perform error checking here
      (goto-char (point-min))
      (if (looking-at "Error")
          (error (buffer-string)))
      )))
 

;;-----------------------------------------------------
;; Keyboard shortcuts:
;;-----------------------------------------------------

; Enable these regardless of ErgoEmacs is on or off
(global-set-key (kbd "C-k") 'kill-line) 
(global-set-key (kbd "C-a") 'beginning-of-visual-line) 
(global-set-key (kbd "C-e") 'end-of-visual-line) 
(global-set-key (kbd "C-c C-g") 'goto-line) 

;; H is for hyper (Menu key)
;; s is for super (Windows key)
(global-set-key (kbd "s-n") 'make-frame-command) 
(global-set-key (kbd "s-c") 'make-frame-command) 
(global-set-key (kbd "s-x") 'delete-frame)
(global-set-key (kbd "H-d") 'delete-frame)
(global-set-key (kbd "H-x") 'maximize-frame)
(global-set-key (kbd "H-n") 'w32-minimize-frame)
(global-set-key (kbd "H-r") 'restore-frame)

(global-set-key (kbd "C-<backspace>") 'backward-kill-word)

(global-set-key (kbd "M-<up>")     'backward-paragraph)
(global-set-key (kbd "M-<down>")   'forward-paragraph)

(global-set-key (kbd "<end>")       'move-end-of-line)
(global-set-key (kbd "<home>")      'move-beginning-of-line)

; Buffer movement
(global-set-key (kbd "M-<right>") 'cycle-buffer)
(global-set-key (kbd "M-<left>")  'cycle-buffer-backward)
(global-set-key (kbd "M-S-<right>") 'cycle-buffer-permissive)
(global-set-key (kbd "M-S-<left>")  'cycle-buffer-backward-permissive)

; Frame movement
(global-set-key (kbd "s-<right>") 'switch-to-next-frame)
(global-set-key (kbd "s-<left>")  'switch-to-previous-frame)
(global-set-key (kbd "C-<tab>")   'switch-to-next-frame)
(global-set-key (kbd "C-S-<tab>") 'switch-to-previous-frame)
(global-set-key (kbd "M-m") 'newline)
 
;; Font height
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-normal-size)
(global-set-key (kbd "C-<kp-add>") 'text-scale-increase)
(global-set-key (kbd "C-<kp-subtract>") 'text-scale-decrease)
(global-set-key (kbd "C-<kp-0>") 'text-scale-normal-size)

;; ErgoEmacs like keys
;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-i") 'previous-line) 
(global-set-key (kbd "M-j") 'backward-char) 
(global-set-key (kbd "M-k") 'next-line) 
(global-set-key (kbd "M-l") 'forward-char) 

(global-set-key (kbd "C-n") 'new-empty-buffer) ; Open New File
(global-set-key (kbd "C-S-n") 'make-frame-command) ; open a new window.
(global-set-key (kbd "C-o") 'find-file) ; Open
(global-set-key (kbd "C-w") 'close-current-buffer) ; Close
;(global-set-key (kbd "C-s") 'save-buffer) ; Save
(global-set-key (kbd "C-S-s") 'write-file) ; Save As
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; isearch
(global-set-key (kbd "M-;") 'isearch-forward)
(global-set-key (kbd "M-'") 'isearch-backward)

;(global-set-key (kbd "M-5") 'query-replace)
;(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "M-R") 'query-replace-regexp)

(global-set-key (kbd "M-1") 'delete-other-windows)
;(global-set-key (kbd "M-!") 'delete-window)

(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-@") 'split-window-horizontally)

; Mark point.
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-A") 'shell-command)

; Window splitting
(global-set-key (kbd "M-s") 'move-cursor-next-pane)
(global-set-key (kbd "M-S") 'move-cursor-previous-pane)

;; Function key bindings
(global-set-key (kbd "<f5>") 'dired)
(global-set-key (kbd "<f6>") 'ibuffer)
(global-set-key (kbd "<f7>") 'compile)
; Don't forget to call visit-tags-table after TAGS generation
(global-set-key (kbd "<f8>") 'run-ctags)
(global-set-key (kbd "<f9>") 'run-doxygen)
;; Google search is bound to C-c g
(global-set-key (kbd "<f10>") 'dim:google)
(global-set-key (kbd "<f11>") 'lookup-wikipedia)
(global-set-key (kbd "<f12>") 'lookup-word-definition)



;;-----------------------------------------------------
;; Check point
;;-----------------------------------------------------

(message "INIT: Completed.")

;; End of file