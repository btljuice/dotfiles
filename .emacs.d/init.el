; Inspired by
; DONE http://milkbox.net/note/single-file-master-emacs-configuration/
; WIP https://github.com/bling/dotemacs
; TODO https://github.com/purcell/emacs.d/blob/master/init.el
; TODO https://github.com/bbatsov/prelude
;
; Emacs keybinding convention
; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
; C-g, C-h, ESC     ; to escape
; M-x list-packages ; example of emacs command. Use Tab for auto-completion
; C-x ...           ; reserved for emacs essential keybindings
; C-c C-letter      ; reserved for major mode
; C-c letter        ; reserved for user
; <F5>-<F9>         ; reserved for user
;

(eval-when-compile (require 'cl))

(defconst *is-a-mac* (eq system-type 'darwin))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (core-directory (concat user-emacs-directory "core/"))
      (bindings-directory (concat user-emacs-directory "bindings/"))
      (config-directory (concat user-emacs-directory "config/")))

    ;;;; Minimal ui
    (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (menu-bar-mode)  ; TMP: Enable it for the moment, for inspiration
    (setq inhibit-startup-screen t)

    (tooltip-mode)

    ;;;; Package configuration
    (require 'package)
    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    ; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)

    ;;;; Better global defaults
    ; TODO create custom.el file if it does not exist
    (setq custom-file (concat user-emacs-directory "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file))

    (defvar btl/essential-packages
      '(
        evil
        ))

    (defun btl/install-essential-packages ()
      "Installs only packages defined in my-packages."
      (interactive)
      (package-refresh-contents)
      (mapc #'(lambda (p)
            (unless (package-installed-p p)
              (package-install p)))
        btl/essential-packages))

    (defmacro btl/after (mode &rest body)
      "`eval-after-load' MODE evaluate BODY."
      (declare (indent defun))
      `(eval-after-load ,mode
         '(progn ,@body)))

    ;;;; emacs lisp
    (defun imenu-elisp-sections ()
      (setq imenu-prev-index-position-function nil)
      (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

    (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

    ;;; from purcell/emacs.d
    (defun require-package (package &optional min-version no-refresh)
      "Install given PACKAGE, optionally requiring MIN-VERSION.
    If NO-REFRESH is non-nil, the available package lists will not be
    re-downloaded in order to locate PACKAGE."
      (if (package-installed-p package min-version)
          t
        (if (or (assoc package package-archive-contents) no-refresh)
            (package-install package)
          (progn
            (package-refresh-contents)
            (require-package package min-version t)))))

    (require-package 'evil)

    (require 'evil)

    (evil-mode t)

    (load custom-file)
    )
