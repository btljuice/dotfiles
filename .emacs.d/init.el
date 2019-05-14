; TODO : .emacs.d into a new repository : dotemacs
; Config Inspired by:
;     DONE http://milkbox.net/note/single-file-master-emacs-configuration/
;     WIP (evil-centric) https://github.com/bling/dotemacs
;     TODO https://github.com/purcell/emacs.d/blob/master/init.el
;     TODO(evil-centric) https://github.com/hlissner/doom-emacs
;     TODO https://github.com/bbatsov/prelude
;     TODO http://wolfecub.github.io/dotfiles/
;
; PACKAGES TO TRY:
;     use-package
;
; TO READ:
;     https://www.gnu.org/software/emacs/manual/eintr.html
;     Mastering Emacs - Mickey Petersen
;     https://www.emacswiki.org/emacs/EmacsNiftyTricks
;
; Startup summary
;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
; Emacs keybinding convention
;     https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;     C-g, C-h, ESC     ; to escape
;     M-x list-packages ; example of emacs command. Use Tab for auto-completion
;     C-x ...           ; reserved for emacs essential keybindings
;     C-c C-letter      ; reserved for major mode
;     C-c letter        ; reserved for user
;     <F5>-<F9>         ; reserved for user
;

(eval-when-compile (require 'cl))

(defconst *is-a-mac* (eq system-type 'darwin))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
		(message "[Emacs initialized in %.3fs]" elapsed)))))

; TODO : optimize gc-cons-threhold
; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (core-directory (concat user-emacs-directory "core/"))
      (bindings-directory (concat user-emacs-directory "bindings/"))
      (config-directory (concat user-emacs-directory "config/")))

   ;;;; Minimal ui
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (menu-bar-mode)  ; TMP: Enable it for the moment, for inspiration
  (tooltip-mode)   ; Show a tooltip line at the bottom of the screen
  (setq inhibit-startup-screen t)
  (setq visible-bell nil)

  ;;;; custom variables
  (defgroup dotemacs nil
    "Custom configuration for dotemacs."
    :group 'local)

  (defcustom dotemacs-cache-directory (concat user-emacs-directory ".cache/")
    "The storage location for various persistent files."
    :type 'directory
    :group 'dotemacs)

  (defcustom dotemacs-completion-engine
    'company
    "The completion engine the use."
    :type '(radio
            (const :tag "company-mode" company)
            (const :tag "auto-complete-mode" auto-complete))
    :group 'dotemacs)

  (defcustom dotemacs-switch-engine
    'helm
    "The primary engine to use for narrowing and navigation."
    :type '(radio
            (const :tag "helm" helm)
            (const :tag "ido" ido)
            (const :tag "ivy" ivy))
    :group 'dotemacs)

  (defcustom dotemacs-pair-engine
    'emacs
    "The primary engine to use auto-pairing and parens matching."
    :type '(radio
            (const :tag "emacs" emacs)
            (const :tag "smartparens" smartparens))
    :group 'dotemacs)

  (defcustom dotemacs-globally-ignored-directories
    '("elpa" ".cache" "target" "dist" "node_modules" ".git" ".hg" ".svn" ".idea")
    "A set of default directories to ignore for anything that involves searching."
    :type '(repeat string)
    :group 'dotemacs)


  ;;;; Package configuration
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-archive-priorities
	'(("gnu" . 10)
	  ("melpa-stable" . 5)
	  ("melpa" . 0)))
  (package-initialize)

  (load (concat core-directory "core-boot"))

  ;;;; Setting custom file 
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;;;; Load all config then all binding files recursively
  ;;;; TODO: I like better to manually specify the load/require expression
  ;;;; TODO: Change load for require
  (cl-loop for file in
	   (append (reverse (directory-files-recursively config-directory "\\.el$"))
		   (reverse (directory-files-recursively bindings-directory "\\.el$")))
	   do (condition-case ex
		  (load (file-name-sans-extension file))
		('error (with-current-buffer "*scratch*"
			  (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex))))))
  )
