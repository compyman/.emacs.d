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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(use-package geiser-guile
  :ensure
  :custom (geiser-guile-load-init-file-p t))
(use-package
  geiser
  :ensure)

(use-package rustic
  :ensure
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

(use-package magit)

(use-package hungry-delete
  :config (global-hungry-delete-mode)
  :custom (hungry-delete-join-reluctantlyis t)
  :ensure t)

(use-package async :ensure t)

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
                    '(font . "Roboto Mono Light 12")
                    )))
(set-face-font 'default "Roboto Mono Light 12")

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
  :hook ((scheme-mode emacs-lisp-mode) . enable-paredit-mode))

;;; Auctex Mode
(use-package tex
  :ensure auctex)

(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-virtualenvwrapper-python "~/.pyenv/versions/3.8.12/bin/python")
  :config (pyvenv-mode 1))

;;; Flycheck Mode
(use-package flycheck
  :ensure
  :hook ((c-mode c++-mode go-mode js-mode)  . flycheck-mode)
  :custom
  (flycheck-clang-language-standard "c++11"))

(use-package flycheck-color-mode-line
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
  :custom
  (undo-tree-visualizer-lazy-drawing 't)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode t))

;;; Avy Mode
(use-package avy
  :bind (("C-;" .  'avy-goto-word-or-subword-1)) )

;;; LSP UI Mode
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :ensure t
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-sideline-show-code-actions nil)
  :after (lsp-mode))

;;; LSP Ivy integration
(use-package lsp-ivy
  :config
  :ensure t
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
  ; (lsp-pyls-plugins-flake8-enabled nil)
  ; (lsp-pylsp-plugins-mccabe-enabled nil)
  ; (lsp-pylsp-plugins-pyflakes-enabled nil)
  ; (lsp-pylsp-plugins-pylint-enabled nil)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)

    ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
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
  :hook
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (lsp-register-custom-settings
   '(("pylsp.plugins.pylsp_mypy.enabled" t t)
     ("pylsp.plugins.pylsp_mypy.live_mode" t t)
     ("pylsp.plugins.pylsp_black.enabled" t t)
     ("pylsp.plugins.pyls_isort.enabled" t t))))


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
  :bind ([remap kill-ring-save] . easy-kill)
  :defer 1)

;;; paradox
;; (use-package paradox
;;   :ensure t
;;   :config (paradox-enable))

;;; smartparens
(use-package smartparens
  :ensure t
  :hook (python-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

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
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bffb799032a7404b33e431e6a1c46dc0ca62f54fdd20744a35a57c3f78586646" "21fb497b14820147b2b214e640b3c5ee19fcadc15bc288e3c16c9c9575d95d66" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "76dc63684249227d64634c8f62326f3d40cdc60039c2064174a7e7a7a88b1587" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "78496062ff095da640c6bb59711973c7c66f392e3ac0127e611221d541850de2" "6a23db7bccf6288fd7c80475dc35804c73f9c9769ad527306d2e0eada1f8b466" "6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" "c620ce43a0b430dcc1b06850e0a84df4ae5141d698d71e17de85e7494377fd81" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "8e5dd88c42089566d5f8e1a23d3017c213eeccd94a7b9e1a58a2dc3e08cb26d5" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" default))
 '(fci-rule-color "#3E4451")
 '(flycheck-javascript-eslint-executable nil)
 '(global-display-line-numbers-mode t)
 '(global-undo-tree-mode t)
 '(indent-tabs-mode nil)
 '(lsp-pyls-server-command '("pylsp"))
 '(lsp-ui-flycheck-enable t)
 '(package-selected-packages
   '(geiser-guile geiser yasnippet-snippets yassnippet-snippets rustic yaml-mode bazel doom-modeline cask-mode cask pyvenv which-key with-venv hungry-delete python-mode solaraized-theme async solarized-theme use-package dired-sidebar easy-kill crux ivy-prescient prescient smartparens dap-mode counsel lsp-ivy lsp-ui lsp-mode projectile paradox flycheck-color-mode-line flycheck undo-tree auctex magit paredit go-eldoc exec-path-from-shell avy rainbow-delimiters elpy))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-spinner-type 'moon)
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
