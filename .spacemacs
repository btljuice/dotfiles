;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; My custom functions
(defsubst btl/windows-p ()
  (memq system-type '(ms-dos windows-nt)))

(defsubst btl/macos-p ()
  (memq system-type '(darwin)))

(defsubst btl/add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun btl/end-string-with (s c)
  (if (equal (substring s -1) c)
      s
    (concat s c)))

(defsubst btl/back->fwdslash (s)
  (btl/end-string-with (replace-regexp-in-string "\\\\" "/" s) "/"))

(defsubst btl/gdrive () (getenv "GDRIVE"))
(defsubst btl/gdrive-fwd () (btl/back->fwdslash (btl/gdrive)))

(defsubst btl/win/binaries32 () (getenv "USER_APP_BINARIES32"))
(defsubst btl/win/binaries64 () (getenv "USER_APP_BINARIES64"))

(defsubst btl/win/binaries32-fwd () (btl/back->fwdslash (btl/win/binaries32)))
(defsubst btl/win/binaries64-fwd () (btl/back->fwdslash (btl/win/binaries64)))

; TODO (defsubst btl/org-folder () )

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
                                 auto-completion-enable-help-tooltip 'manual
                                 auto-completion-enable-sort-by-usage t)
     smex
     lsp

     (shell :variables shell-default-height 33
            shell-default-position 'top
            shell-default-shell 'ansi-term)
     fasd

     cscope
     gtags
     dash

     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      flycheck-scalastylerc "~/code/scala_style.xml")
     (semantic :variables semanticdb-find-default-throttle '(file))
     ;; typography

     version-control
     git
     ;;perforce
     ;jira

     ;; better-defaults ; For emacs key-bindings only
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
     ;; org-clock-csv
     finance

     ;;clojure
     emacs-lisp
     ;;scheme
     ;;common-lisp
     ;; (latex :variables latex-enable-folding t
     ;;                   latex-enable-auto-fill t)
     ;; sql
     html
     (scala :variables scala-backend 'scala-metals
            scala-auto-insert-asterisk-in-comments t
            ;; scala-use-unicode-arrows t
            scala-auto-start-backend t)
     ;; (c-c++ :variables c-c++-enable-clang-support t)
     ; try emacs-cquery
     ;java
     (python :variables python-fill-column 96)
     ;ipython-notebook
     ;lua
     ;sagemath
     ;; octave
     ;ess  ; R

     shell-scripts
     ;; windows-scripts
     autohotkey
     vimscript
     csv

     restclient
     html
     javascript

     ;; keyboard-layout ;; for dvorak

     ;erc ;; irc layer
     ;games
     themes-megapack
     ;; xkcd
     selectric
     colors
     ;; (colors :variables
     ;;         colors-enable-rainbow-identifiers t)
     ;; pdf-tools

     ;;;; Interesting layers to try eventually
     ;; wakatime ;; A time tracker in emacs
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(yasnippet-snippets)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(exec-path-from-shell)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
  ;; `used-only' installs only explicitly used packages and deletes any unused
  ;; packages as well as their unused dependencies. `used-but-keep-unused'
  ;; installs only the used packages but won't delete unused ones. `all'
  ;; installs *all* packages supported by Spacemacs and never uninstalls them.
  ;; (default is `used-only')
  dotspacemacs-install-packages 'used-only)

  (message "dotspacemacs/layers END")
  )

(defun dotspacemacs/user-env ()
"Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
(spacemacs/load-spacemacs-env)
)

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.

  (message "dotspacemacs/init BEGIN ")

   (setq-default
    ;; If non-nil then enable support for the portable dumper. You'll need
    ;; to compile Emacs 27 from source following the instructions in file
    ;; EXPERIMENTAL.org at to root of the git repository.
    ;; (default nil)
    dotspacemacs-enable-emacs-pdumper nil

    ;; Name of executable file pointing to emacs 27+. This executable must be
    ;; in your PATH.
    ;; (default "emacs")
    dotspacemacs-emacs-pdumper-executable-file "emacs"

    ;; Name of the Spacemacs dump file. This is the file will be created by the
    ;; portable dumper in the cache directory under dumps sub-directory.
    ;; To load it when starting Emacs add the parameter `--dump-file'
    ;; when invoking Emacs 27.1 executable on the command line, for instance:
    ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
    ;; (default spacemacs.pdmp)
    dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((todos . 5)
                                (agenda . 5)
                                (bookmarks . 5)
                                (recents . 10)
                                (projects . 5))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         monokai
                         spacemacs-light
                         spacemacs-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Menlo" ; "Source Code Pro"
                               :size 12
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
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

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   )
  (message "dotspacemacs/init END")
  )

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (message "dotspacemacs/user-init BEGIN")

  ;; (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)

  (when (btl/windows-p)
    ;(setq-default exec-path (append exec-path '("c:\\msys64\\mingw64\\bin")))
    ;(custom-set-variables '(helm-ag-base-command "c:\\msys64\\mingw64\\bin\\ag.exe --vimgrep"))

      (setenv "PATH" (concat (btl/win/binaries64) "everything;"
                             (btl/win/binaries64) "git\\cmd;"
                             (btl/win/binaries64) "ledger;"
                             (btl/win/binaries64) "platinum-searcher;"
                             (btl/win/binaries64) "ffmpeg\\bin;"
                             (btl/win/binaries64) "imagemagick;"
                             (btl/win/binaries32) "cscope;"
                             (btl/win/binaries32) "ctags;"
                             (btl/win/binaries32) "gtags;"
                             (getenv "PATH")))

      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "everything"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "git/cmd"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "ledger"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "platinum-searcher"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "ffmpeg/bin"))
      (add-to-list 'exec-path (concat (btl/win/binaries64-fwd) "imagemagick"))
      (add-to-list 'exec-path (concat (btl/win/binaries32-fwd) "cscope"))
      (add-to-list 'exec-path (concat (btl/win/binaries32-fwd) "ctags"))
      (add-to-list 'exec-path (concat (btl/win/binaries32-fwd) "gtags"))
    )

  (when (btl/macos-p)
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (add-to-list 'exec-path "/usr/local/bin"))

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
  (add-hook 'markdown-mode 'auto-fill-mode)

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

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (message "dotspacemacs/user-config BEGIN")
  ; hotfix for helm-bookmark-map is void
  (require 'helm-bookmark)
  (setq projectile-enable-caching t)
  (setq-default avy-all-windows 'all-frames
                truncate-lines t
                )
  (spacemacs|disable-company eshell-mode)
  ;; (spacemacs|disable-company LaTeX/MPS)
  (spacemacs/toggle-highlight-current-line-globally-on)
  (global-prettify-symbols-mode 1)
  (blink-cursor-mode 1)
  (auto-fill-mode -1)
  (setq-default fill-column 120)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default require-final-newline 'visit-save)
  (setq-default case-replace t)
  (setq-default delete-trailing-lines t)
  (setq-default show-trailing-whitespace t)
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq magit-repository-directories '(("~/code/" . 1)))

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-show-all-today t)
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WIP(w)" "PENDING(p)" "|" "DONE(d)" "CANCELLED(c)" )))
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
    ;; org-agenda-files should be customized instead
    ;; (setq org-agenda-files '("/doc/org"))
    (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                   (file+headline "~/Dropbox/doc/org/inbox.org" "Task")
                                   "* TODO %i%?")
                                  ("e" "Event [inbox]" entry
                                   (file+headline "~/Dropbox/doc/org/inbox.org" "Event")
                                   "* %i%?")
                                  ("n" "Note/Link [inbox]" entry
                                   (file+headline "~/Dropbox/doc/org/inbox.org" "Note/Link")
                                   "* %i%?")
                                  ))
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-log-mode-items '(closed state clock))  ; state and clock possible
    (setq org-agenda-start-with-log-mode t)
    (setq org-refile-targets '((nil :maxlevel . 9)
                               ("~/Dropbox/doc/org/agenda.org" :maxlevel . 9)
                               ("~/Dropbox/doc/org/someday.org" :maxlevel . 9)
                               ("~/Dropbox/doc/org/objective.org":maxlevel . 9)))
    (setq org-duration-format '(("h") (special . 2)))
    ;; (define-key org-agenda-mode-map "K" 'org-habit-toggle-habits)
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "k" 'org-habit-toggle-habits)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ttl" 'org-toggle-latex-fragment)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
    (setq org-log-state-notes-into-drawer "LOGSTATE")
    (setq org-clock-into-drawer "LOGCLOCK")
    )

  ; Ledger settings
  (setq ledger-highlight-xact-under-point nil) ; Prevents hightlight of the current transaction
  (setq ledger-report-links-in-register nil) ; Prevents ledger for prepending transaction linecode. For my specific implementation, it puts the absolute path, which is annoying
  (add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))
  (setq ledger-reports
     '(("bal assets liabilities" "%(binary) -f %(ledger-file) bal assets or liabilities or acompte")
      ("bal income expenses" "%(binary) -f %(ledger-file) bal income or expenses")
      ("bal income expenses this month" "%(binary) -f %(ledger-file) --period \"this month\"  bal income or expenses")
      ("bal income expenses last month" "%(binary) -f %(ledger-file) --period \"last month\"  bal income or expenses")
      ("bal" "%(binary) -f %(ledger-file) bal")
      ("reg" "%(binary) -f %(ledger-file) reg")
      ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
      ("account" "%(binary) -f %(ledger-file) reg %(account)")
      ("account this month" "%(binary) -f %(ledger-file) --period \"this month\" reg %(account)")
      ("account last month" "%(binary) -f %(ledger-file) --period \"last month\" reg %(account)")))



  ; jira mode
  ;; (setq-default jiralib-url "https://float4-jira.atlassian.net/secure/Dashboard.jspa")

  ;; It seems that (server-start) in .emacs.d/init.el does not work correctly
  ;; Manually restarting the server here.
  ;; (server-start)

  ;; Make evil-mode up/down operate in screen lines instead of logical lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; Move around with control hjkl
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (setq-default evil-escape-key-sequence "jk")
  ;; (define-key evil-normal-state-map (kbd "DEL") 'spacemacs/smex)

  ;; TODO Move to a hopper specific location
  (add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-javaprop-mode))
  (add-hook 'conf-javaprop-mode-hook #'hs-minor-mode)

  ;; To fold xml files and probably something about comments
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))
  (add-hook 'nxml-mode-hook 'hs-minor-mode)

  (setq custom-file "~/.spacemacs.custom.el")
  (load-file custom-file)

  (message "dotspacemacs/user-config END")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
