;;; packages.el --- sagemath layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: btljuice <btljuice@btl-samsung-ltp>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `sagemath-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sagemath/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sagemath/pre-init-PACKAGE' and/or
;;   `sagemath/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sagemath-packages
  '(
    sage-shell-mode
    eldoc
    helm-sage
    auto-complete-sage
    ;; ob-math
    )
  "The list of Lisp packages required by the sagemath layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"
  )

(defun sagemath/init-sage-shell-mode ()
  (use-package sage-shell-mode
    :defer t
    :config
    (progn

      (spacemacs/set-leader-keys-for-major-mode 'sage-shell-mode
        "c" 'sage-shell:interrupt-subjob
        "o" 'sage-shell:delete-output
        "O" 'sage-shell:clear-current-buffer
        "l" 'sage-shell:load-file
        "?" 'sage-shell:help
        "b" 'sage-shell:list-outputs
        "w" 'sage-shell:copy-previous-output-to-kill-ring
        "hh" 'helm-sage-complete
        "p#" 'sage-shell-blocks:pull-next
        )
      ;; (evil-define-key 'insert 'sage-shell-mode-map (kbd "tab") 'helm-sage-complete)
      ;; Enable setting of executable
      (setq sage-shell:completion-function 'helm-sage)
      (spacemacs/declare-prefix-for-mode 'sage-shell:sage-mode "ms" "send to REPL")
      (spacemacs/set-leader-keys-for-major-mode 'sage-shell:sage-mode
        "'" 'sage-shell:run-sage ;; TODO: create a sage shell and/or switch to associated shell
        "?" 'sage-shell:help
        "si" 'sage-shell:run-new-sage
        "sb" 'sage-shell-edit:send-buffer
        "sB" 'sage-shell-edit:send-buffer-and-go
        "sr" 'sage-shell-edit:send-region
        "sR" 'sage-shell-edit:send-region-and-go
        "sl" 'sage-shell-edit:send-line*
        "sL" 'sage-shell-edit:send-line-and-go
        "s#" 'sage-shell-blocks:send-current
        "p" 'sage-shell-edit:pop-to-process-buffer)

      (evil-define-key 'normal sage-shell:sage-mode-map
        (kbd "] #") 'sage-shell-blocks:forward
        (kbd "[ #") 'sage-shell-blocks:backward)

      ;; is this working?
      (setq sage-shell:input-history-cache-file "~/.emacs.d/.cache/.sage_shell_input_history")
      )))

(defun sagemath/init-helm-sage ()
  (use-package helm-sage
    :defer t
    ))

(defun sagemath/init-auto-complete-sage ()
  (use-package auto-complete-sage
    :defer t
    ))

(defun sagemath/post-init-eldoc()
  ;; Turn on eldoc-mode in Sage terminal and in Sage source files
  (add-hook 'sage-shell-mode #'eldoc-mode)
  (add-hook 'sage-shell:sage-mode-hook #'eldoc-mode)
  )

;;; packages.el ends here
