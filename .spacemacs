;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; My custom functions
(defsubst btl/windows-p ()
  (memq system-type '(ms-dos windows-nt)))

(defsubst btl/add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defsubst btl/back->fwdslash (s) (replace-regexp-in-string "\\\\" "/" s))

(defsubst btl/gdrive () (getenv "GDRIVE"))
(defsubst btl/gdrive-fwd () (btl/back->fwdslash (btl/gdrive)))


(defsubst btl/win/binaries32 () (concat (getenv "GDRIVE") "\\computer\\app\\win\\free\\binaries32\\"))
(defsubst btl/win/binaries64 () (concat (getenv "GDRIVE") "\\computer\\app\\win\\free\\binaries64\\"))

(defsubst btl/win/binaries32-fwd () (replace-regexp-in-string "\\\\" "/" (btl/win/binaries32)))
(defsubst btl/win/binaries64-fwd () (replace-regexp-in-string "\\\\" "/" (btl/win/binaries64)))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."

  (message "dotspacemacs/layers BEGIN")

  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;ivy
     helm
     (auto-completion :variables auto-completion-return-key-behavior nil
                                 auto-completion-tab-key-behavior nil
                                 auto-completion-enable-snippets-in-popup t
                                 auto-completion-enable-help-tooltip t)
     smex

     cscope
     gtags
     (shell :variables shell-default-height 16
            shell-default-position 'bottom
            shell-default-shell 'shell)

     ;; better-defaults ; For emacs key-bindings only
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     ;semantic
     typography

     version-control
     git
     perforce
     ;jira

     dash

     evil-snipe
     vinegar
     ;; evil-cleverparens
     ;; vim-empty-lines
     ;; skewer-mode

     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     ranger
     imenu-list

     markdown
     org
     org-clock-csv
     finance

     emacs-lisp
     common-lisp
     latex
     c-c++
     java
     python
     lua

     shell-scripts
     windows-scripts
     autohotkey
     vimscript
     csv

     restclient
     html
     javascript

     ;; keyboard-layout ;; for dvorak 

     ;erc ;; irc layer
     ;games
     ;themes-megapack

     ;;;; Interesting layers to try eventually
     ;; wakatime ;; A time tracker in emacs
     ;; fasd ;; Quickly access files through shell
     ;; cscope
     ;; :gtags
     ;; (colors :variables
     ;;         colors-enable-rainbow-identifiers t)
     ;;;;
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only)

  (message "dotspacemacs/layers END")
  )

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.

  (message "dotspacemacs/init BEGIN ")

  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 5)
                                (bookmarks . 10)
                                (agenda . 5)
                                (todos . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         spacemacs-dark
                         spacemacs-light
                         wombat
                         molokai
                         deeper-blue
                         tango-2
                         zenburn
                         wilson
                         zonokai-blue
                         twilight
                         wheatgrass
                         zen-and-art
                         alect-black
                         alect-black-alt
                         twilight-anti-bright)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling nil
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   )
  (message "dotspacemacs/init END")
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (message "dotspacemacs/user-init BEGIN")

  (when (btl/windows-p)
    ;(setq-default exec-path (append exec-path '("c:\\msys64\\mingw64\\bin")))
    ;(custom-set-variables '(helm-ag-base-command "c:\\msys64\\mingw64\\bin\\ag.exe --vimgrep"))

      (setenv "PATH" (concat (btl/win/binaries64) "everything;"
                             (btl/win/binaries64) "git\\cmd;"
                             (btl/win/binaries64) "ledger;"
                             (btl/win/binaries64) "platinum-searcher;"
                             (btl/win/binaries64) "ffmpeg-2.4;"
                             (btl/win/binaries32) "cscope;"
                             (btl/win/binaries32) "ctags;"
                             (btl/win/binaries32) "gtags;"
                             (getenv "PATH")))

      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "everything"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "git/cmd"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "ledger"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "platinum-searcher"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "ffmpeg-2.4"))
      (add-to-list 'exec-path (concat (btl/win/binaries32-fwd) "cscope"))
      (add-to-list 'exec-path (concat (btl/win/binaries32-fwd) "ctags"))
      (add-to-list 'exec-path (concat (btl/win/binaries32-fwd) "gtags"))
    )

  (setq-default
   ;; Evil
   evil-shift-round nil

   ;; whitespace-style '(face tabs tab-mark newline-mark)
   ;; whitespace-display-mappings '((newline-mark 10 [172 10])
   ;;                               (tab-mark 9 [9655 9]))

   ;; Customize your theme
   )

  (setq backup-directory-alist '((".*" . "~/.emacs.d/private/backup")))

  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'makefile-mode  'whitespace-mode)

  (let ((comint-hooks '(eshell-mode-hook
                        term-mode-hook
                        messages-buffer-mode-hook
                        inferior-emacs-lisp-mode-hook)))
    (btl/add-to-hooks (lambda () (setq-local global-hl-line-mode nil)) comint-hooks)
    (btl/add-to-hooks (lambda () (setq-local scroll-margin 0))         comint-hooks)
    )

  (when (file-exists-p "~/spacemacs.local.el")
    (load "~/spacemacs.local.el")
    )

  ;;;; Spacemacs cheatsheet
  ;; SPC-s-s Swoop file
  ;; SPC-p-* Projectile
  ;; [] for next/previous unimpaired command
  ;; ys or yS for add-surround
  ;; cs or cS for change-surround
  ;; ds or dS for delete-surround
  ;; SPC-v/V for contract/expand
  ;; M-: for Eval
  ;; SPC-m-e-e eval-last-sexp
  ;; SPC-: emacs-ex
  ;; SPC-l : layouts
  ;; SPC-y
  ;; SPC-SPC

  (message "dotspacemacs/user-init END")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (message "dotspacemacs/user-config BEGIN")
  (setq projectile-enable-caching t)
  (setq-default avy-all-windows 'all-frames
                truncate-lines t
                )
  (spacemacs|disable-company eshell-mode)
  (spacemacs|disable-company LaTeX/MPS)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (blink-cursor-mode 1)

  (with-eval-after-load 'org
    (setq org-todo-keywords '((sequence "TODO" "PENDING" "WIP" "|" "DONE" "WONTFIX")))
    (setq spaceline-org-clock-p t)
    (setq spaceline-org-clock-format-function
          '(lambda ()
             (let ((s (org-clock-get-clock-string)))
               (if (< 40 (length s) )
                   (concat (substring s 0 40) "..." )
                 s
                 )
               )
             ))
    (setq org-agenda-files (list (concat (btl/gdrive-fwd) "/todo.org")
                                 (concat (btl/gdrive-fwd) "/float4.org")
                                 (concat (btl/gdrive-fwd) "/personnel.org")))
    (setq org-time-clocksum-use-fractional t)
    )

  ; Ledger settings
  (setq ledger-highlight-xact-under-point nil) ; Prevents hightlight of the current transaction
  (setq ledger-report-links-in-register nil) ; Prevents ledger for prepending transaction linecode. For my specific implementation, it puts the absolute path, which is annoying
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

  ; jira mode
  (setq-default jiralib-url "https://float4-jira.atlassian.net/secure/Dashboard.jspa")

  ;; It seems that (server-start) in .emacs.d/init.el does not work correctly
  ;; Manually restarting the server here.
  (server-start)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
  ;; (define-key evil-normal-state-map (kbd "DEL") 'spacemacs/smex)

  ;; (spacemacs/toggle-evil-cleverparens-on)
  ;; (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
  ;; (add-hook 'emacs-lisp-hook #'evil-cleverparens-mode)
  
  (message "dotspacemacs/user-config END")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#49483E" t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(ledger-reports
   (quote
    (("bal bnc argent acompte" "%(binary) -f %(ledger-file) bal bnc argent acompte")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")
     ("bal by month" "%(binary) -M --period-sort \"(amount)\" -f %(ledger-file) bal"))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (winum restclient-helm ob-restclient fuzzy company-restclient know-your-http-well org-clock-csv wgrep typo ivy-hydra helm-gtags helm-cscope xcscope ggtags flyspell-correct-ivy counsel-projectile counsel-dash counsel swiper ivy zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme sunny-day-theme subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monochrome-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme ledger-mode flycheck-ledger csv-mode org-jira org org-plus-contrib multiple-cursors git-link flyspell-correct-helm flyspell-correct flycheck eyebrowse dumb-jump column-enforce-mode auto-complete auctex anaconda-mode eclim anzu iedit smartparens highlight undo-tree git-gutter yasnippet helm helm-core magit git-commit with-editor async projectile js2-mode company dash org-projectile pcache org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot imenu-list evil-cleverparens paredit py-yapf zonokai-theme zenburn-theme zeal-at-point xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe use-package twilight-anti-bright-theme tango-2-theme tagedit sublime-themes stickyfunc-enhance srefactor spacemacs-theme spaceline smooth-scrolling smex smeargle slime slim-mode skewer-mode shell-pop scss-mode sass-mode restclient restart-emacs ranger rainbow-delimiters quelpa pyvenv pytest pyenv-mode powershell popwin pip-requirements persp-mode pcre2el paradox page-break-lines pacmacs p4 orgit open-junk-file neotree multi-term move-text monokai-theme molokai-theme mmm-mode markdown-toc magit-gitflow lua-mode lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore helm-flyspell helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-gutter-fringe+ gh-md flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode emacs-eclim elisp-slime-nav disaster diff-hl define-word dactyl-mode cython-mode company-web company-tern company-statistics company-quickhelp company-c-headers company-auctex company-anaconda coffee-mode cmake-mode clean-aindent-mode clang-format buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk ahk-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell 2048-game)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
