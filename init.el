;; init.el --- Milkmacs configuration file
;;;; Emacs Settings
;; Turn off mouse interface early in startup to avoid momentary display
;;;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;;; (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; NEVER GARBAGE COLLECT
(setq read-process-output-max (* 3 1024 1024)) ;; 1mb
;; quiet!!
(setq ring-bell-function 'ignore)
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))
(setopt use-short-answers t)
;; general display niceness
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;;set backup behavior
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
;; set auth-sources to use an encrypted file
(setq auth-sources '((:source "~/.authinfo.gpg")))

;; package.el
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(use-package eglot
  :custom
  (eglot-report-progress t)
  (eglot-autoshutdown t)
  (eglot-connect-timeout nil)
  (eglot-extend-to-xref t)
  (eglot-sync-connect 5)
  :config
  (add-to-list
   'eglot-server-programs
   '((python-ts-mode python-mode) . ("uv" "tool" "run" "--with" "python-lsp-ruff" "--from" "python-lsp-server" "pylsp")))
  (add-to-list
   'eglot-server-programs
   '((kotlin-ts-mode kotlin-mode). ("bash" "/opt/homebrew/Cellar/kotlin-lsp/0.253.10629/libexec/kotlin-lsp.sh" "--stdio")))
  )

(use-package copilot
  :ensure t)
(use-package copilot-chat :ensure t)

(define-key global-map (kbd "C-z") (make-sparse-keymap))

(use-package eca :ensure t)
(use-package mcp
  :ensure t
  :custom (mcp-hub-servers
           '(("rovo" . (:command "npx"
                        :args ("-y" "mcp-remote" "https://mcp.atlassian.com/v1/sse"))))))







(use-package geiser-guile
  :ensure t
  :custom (geiser-guile-load-init-file-p t))
(use-package geiser :ensure t)

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package which-key
  :config (which-key-mode)
  :ensure t)

(use-package magit  :ensure t)

(use-package hungry-delete
  :config (global-hungry-delete-mode)
  :custom (hungry-delete-join-reluctantlyis t)
  :ensure t)

(use-package async :ensure t)


;; Font and frame size
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Fira Code 10")
                    )))


;; ;; kotlin IDE
;; (use-package kotlin-mode
;;   :after (dap-mode)
;;   :config
;;   (require 'dap-kotlin)
;;   ;; should probably have been in dap-kotlin instead of lsp-kotlin
;;   (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
;;   :hook
;;   (kotlin-mode . lsp))


(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))


;;; prog minor modes
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'eldoc-mode)
;;; show-paren-mode
(setq show-paren-style 'parenthesis)
(add-hook 'prog-mode-hook 'show-paren-mode)
;;; Built in Emacs Lisp
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

(use-package eldoc
  :hook ((prog-mode-hook lisp-interaction-mode-hook) . eldoc-mode)
  )

(use-package flymake
  :hook ((prog-mode-hook) . eldoc-mode)
  :bind (
         :map flymake-mode-map
         ("M-n" . 'flymake-goto-prev-error)
         ("M-p" . 'flymake-goto-next-error)))


;;UNDO TREE

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
;;; Paredit Mode
(use-package paredit
  :ensure t
  :hook ((scheme-mode emacs-lisp-mode lisp-data-mode) . enable-paredit-mode))

;;; Auctex Mode
(use-package tex
  :ensure auctex)

(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-virtualenvwrapper-python "~/.pyenv/versions/3.11.6/bin/python")
  :config (pyvenv-mode 1))


;;;; in mac add shell path to emacs exec path
(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :custom
  (exec-path-from-shell-name "zsh")
  (exec-path-from-shell-variables '("LANG"
                                    "PATH" "MANPATH"
                                    "LDFLAGS" "CPPFLAGS" "PKG_CONFIG_PATH"
                                    "GITHUB_TOKEN"
                                    "CIRCLECI_TOKEN"
                                    "DATADOG_TOKEN"
                                    "SENDWAVE_HOST" "WAVE_URL_ROOT" "PLUGIN_URL_ROOT"
                                    "MYPYCACHEDIR"
                                    "PYTHONPYCACHEPREFIX"
                                    "PYENV_ROOT"
                                    "PYENV_SHELL"
                                    "PYENV_VIRTUALENV_INIT"
                                    "WORKON_HOME"
                                    "VIRTUALENVWRAPPER_PROJECT_FILENAME"
                                    "VIRTUALENVWRAPPER_ENV_BIN_DIR"
                                    "PYENV_VIRTUALENVWRAPPER_PYENV_VERSION"
                                    "VIRTUALENVWRAPPER_PROJECT_CD"
                                    "VIRTUALENVWRAPPER_PYTHON"
                                    "VIRTUALENVWRAPPER_VIRTUALENV"
                                    "VIRTUALENVWRAPPER_VIRTUALENV_CLONE"
                                    "VIRTUALENVWRAPPER_SCRIPT"
                                    "VIRTUALENVWRAPPER_LAZY_SCRIPT"
                                    "_VIRTUALENVWRAPPER_API"
                                    "VIRTUALENVWRAPPER_HOOK_DIR"
                                    "VIRTUAL_ENV_DISABLE_PROMPT"
                                    ))
  :config (exec-path-from-shell-initialize))

;;;;Org-mode
(define-key global-map (kbd  "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("S" "Standup" item (file+olp+datetree "~/org/work-agenda.org" "Standup"))))
(setq org-agenda-files (list "~/org/" "~/.notes"))
(setq org-log-done t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;;flymake


;; DOOM

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  :config
  (setq nerd-icons-font-family "DejaVuSansM Nerd Font Mono")
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'doom-solarized-light t))
      ('dark (load-theme 'doom-tokyo-night t))))
   (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))



(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; Avy Mode
(use-package avy
  :ensure t
  :bind (("C-;" .  'avy-goto-word-or-subword-1)) )

;;; ace window
(use-package ace-window
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets :ensure t)


;;  Debug Adaptor Protocol Mode
;; (use-package dap-mode
;;   :custom (dap-python-debugger 'debugpy)
;;   :config
;;   (require 'dap-python)
;;   (defun dap-python--pyenv-executable-find (command)
;;     (executable-find command)))


;;; Handy CRUX addons
(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("s-r" . crux-recentf-find-file)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ([(shift return)] . crux-smart-open-line))
  :config
  (crux-reopen-as-root-mode)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region))

(use-package easy-kill
  :bind ([remap kill-ring-save] . easy-kill))

;;; smartparens
(use-package smartparens
  :ensure t
  :hook (python-ts-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package terraform-mode
  :ensure t
  :defer t
  :mode ("\\.tf\\'"))

;;; projectile mode
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :bind (
         :map projectile-mode-map
              ("S-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :ensure t
  :bind ((:map minibuffer-local-map
               ("M-A" . marginalia-cycle))
         (:map completion-list-mode-map
               ("M-A" . marginalia-cycle)))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
;; Example configuration for Consult

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element


  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4.el (projectile-project-root)
;;   (autoload 'projectile-project-root "projectile")
;;  (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )


;; A few more useful configurations...

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.

  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; TAB cycle if there are only few candidates
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
)



;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...

;; tramp config
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :machine "server")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)
(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))


					; END OF USER CONFIG

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(calendar-week-start-day 1)
 '(connection-local-criteria-alist
   '(((:application vc-git) vc-git-connection-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine
                    "Nathan-Rosenbloom-XPW326CYG0-SW.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((vc-git-connection-default-profile (vc-git--program-version))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . tramp-ps-time)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (ttname . string)
                                          (time . tramp-ps-time)
                                          (nice . number)
                                          (etime . tramp-ps-time)
                                          (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string)
                                          (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . number)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))))
 '(corfu-quit-no-match t)
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307"
     "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce"
     default))
 '(geiser-guile-load-init-file t nil nil "Customized with use-package geiser-guile")
 '(geiser-repl-current-project-function 'projectile-project-root)
 '(global-display-line-numbers-mode t)
 '(indent-tabs-mode nil)
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(a68-mode ace-window aider async auctex cfrs consult copilot
              copilot-chat corfu crux direnv doom-modeline doom-themes
              eat eca eglot eglot-java exec-path-from-shell
              geiser-guile geiser-overlay gradle-mode hungry-delete
              hydra ivy jsonrpc kotlin-mode kotlin-ts-mode ligature lv
              marginalia orderless org-jira paredit posframe
              projectile pyvenv ripgrep smartparens terraform-mode
              treesit-auto undo-tree vertico which-key
              yasnippet-snippets))
 '(safe-local-variable-values
   '((pyvenv-workon . remit-ide) (checkdoc-minor-mode . t)
     (pyvenv-workon . remit) (pyvenv-workon . "frontplugin")
     (major-mode . yaml-mode) (pyvenv-workon . "remit3610")
     (elpy-project-root . "\\./") (elpy-project-root . \./)
     (elpy-project-root "./") (pyvenv-workon "remit")))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((use-package) (use-package)))
 '(warning-suppress-types '((use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
