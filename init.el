;; init.el --- Milkmacs configuration file
;;;; Emacs Settings
;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; NEVER GARBAGE COLLECT
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024)) ;; 1mb
;; quiet!!
(setq ring-bell-function 'ignore)
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))

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
             '("melpa" . "https://melpa.org/packages/") t)
(use-package geiser-guile
  :ensure t
  :custom (geiser-guile-load-init-file-p t))
(use-package geiser :ensure t)

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(use-package which-key
  :config (which-key-mode)
  :ensure t)

(use-package magit
  :ensure t)

(use-package hungry-delete
  :config (global-hungry-delete-mode)
  :custom (hungry-delete-join-reluctantlyis t)
  :ensure t)

(use-package async :ensure t)
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-city-lights t))
(use-package solarized-theme
  :ensure t
  :config
  ;;; Apply Theme on system-appearance change
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'solarized-light-high-contrast t))
      ('dark (load-theme 'solarized-dark-high-contrast t))))
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))


;; Font and frame size
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Fira Code 10")
                    )))


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


;;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook ((scheme-mode emacs-mode) . rainbow-delimiters-mode))

;;; prog minor modes
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
;;; show-paren-mode
(setq show-paren-style 'parenthesis)
(add-hook 'prog-mode-hook 'show-paren-mode)
;;; Built in Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)

;;; Paredit Mode
(use-package paredit
  :ensure t
  :hook ((scheme-mode emacs-lisp-mode) . enable-paredit-mode))

;;; Auctex Mode
(use-package tex
  :ensure auctex)

(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-virtualenvwrapper-python "~/.pyenv/versions/3.8.13/bin/python")
  :config (pyvenv-mode 1))

;;; Flycheck Mode
(use-package flycheck
  :ensure t
  :hook ((c-mode c++-mode go-mode js-mode)  . flycheck-mode)
  :custom
  (flycheck-clang-language-standard "c++11"))

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode)
  :after (flycheck))

;;; Counsel & Ivy
(use-package counsel
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (ivy-prescient-mode)
  :bind (("C-s" .  'swiper-isearch)
         ("M-x" .  'counsel-M-x)
         ("C-x C-f" . 'counsel-find-file)
         ("M-y" . 'counsel-yank-pop)
         ("<f1> f" . 'counsel-describe-function)
         ("<f1> v" . 'counsel-describe-variable)
         ("<f1> l" . 'counsel-find-library)
         ("<f2> i" . 'counsel-info-lookup-symbol)
         ("<f2> u" . 'counsel-unicode-char)
         ("<f2> j" . 'counsel-set-variable)
         ("C-x b" . 'ivy-switch-buffer)
         ("C-c v" . 'ivy-push-view)
         ("C-c V" . 'ivy-pop-view))
  :after (ivy ivy-prescient swiper))
(use-package ivy
  :ensure t)

(use-package prescient
  :ensure t)

(use-package ivy-prescient
  :ensure t
  :after (prescient))

(use-package swiper
  :ensure t)

;;;; in mac add shell path to emacs exec path
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :custom
  ;(exec-path-from-shell-arguments '("--login" "-c"))
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

(use-package company
  :ensure t
  :config
  (global-company-mode)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1))

;;;;Org-mode
(define-key global-map (kbd  "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-agenda-files (list "~/org/work-agenda.org"))
(setq org-log-done t)
;;;;
(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))
;;;;Undo Tree Mode

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-visualizer-lazy-drawing 't)
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))
  :config
  (global-undo-tree-mode t))

;;; Avy Mode
(use-package avy
  :ensure t
  :bind (("C-;" .  'avy-goto-word-or-subword-1)) )

;;; LSP UI Mode
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :ensure t
  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-sideline-show-code-actions nil)
  :after (lsp-mode))

;;; LSP Ivy integration
(use-package lsp-ivy
  :ensure t
  :config
  (define-key lsp-mode-map
              [remap xref-find-apropos]
              #'lsp-ivy-workspace-symbol)
  :after (lsp-mode))

;;; LSP Mode
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


(use-package yasnippet-snippets :ensure t)

(use-package lsp-mode
  :bind ("M-j" . lsp-ui-imenu)
  :ensure t
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)

  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (read-process-output-max (* 1024 1024))
  (lsp-keymap-prefix "M-l")
  (lsp-modeline-diagnostics-mode t)
  (lsp-signature-auto-activate nil)
  :hook
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))


;;; Debug Adaptor Protocol Mode
(use-package dap-mode
  :ensure t
  :custom (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  (defun dap-python--pyenv-executable-find (command)
    (executable-find command)))

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
  :hook (python-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package terraform-mode
  :defer t
  :mode ("\\.tf\\'"))

;;; projectile mode
(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :bind (
         :map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))



; END OF USER CONFIG
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(calendar-week-start-day 1)
 '(custom-safe-themes
   '("2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" default))
 '(fci-rule-color "#3E4451")
 '(geiser-guile-load-init-file t nil nil "Customized with use-package geiser-guile")
 '(global-display-line-numbers-mode t)
 '(indent-tabs-mode nil)
 '(lsp-disabled-clients '(pyls))
 '(lsp-document-sync-method nil)
 '(package-selected-packages
   '(lsp-docker lsp-treemacs terraform-mode teraform-mode doom-themes geiser-guile geiser yasnippet-snippets yassnippet-snippets rustic yaml-mode bazel doom-modeline cask-mode cask pyvenv which-key with-venv hungry-delete python-mode solaraized-theme async solarized-theme use-package dired-sidebar easy-kill crux ivy-prescient prescient smartparens dap-mode counsel lsp-ivy lsp-ui lsp-mode projectile paradox flycheck-color-mode-line flycheck undo-tree auctex magit paredit go-eldoc exec-path-from-shell avy rainbow-delimiters elpy))
 '(python-shell-interpreter "jupyter")
 '(python-shell-interpreter-args "\"console --simple-prompt\"")
 '(python-shell-prompt-detect-failure-warning nil)
 '(safe-local-variable-values
   '((checkdoc-minor-mode . t)
     (pyvenv-workon . remit)
     (pyvenv-workon . "frontplugin")
     (major-mode . yaml-mode)
     (pyvenv-workon . "remit3610")
     (elpy-project-root . "\\./")
     (elpy-project-root . \./)
     (elpy-project-root "./")
     (pyvenv-workon "remit")))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(undo-tree-visualizer-diff t)
 '(warning-suppress-log-types '((use-package) (use-package)))
 '(warning-suppress-types '((use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
