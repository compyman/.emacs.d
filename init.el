(setq ring-bell-function 'ignore)
;; init.el --- Milkmacs configuration file
;;;; Emacs Settings
;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  ;;  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;;  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (global-set-key [kp-delete] 'delete-char))

;;set backup behavior
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; set auth-sources to use an encrypted file
(setq auth-sources '((:source "~/.authinfo.gpg")))

;;;; package.el
(setq package-dir (concat (file-name-directory user-init-file) "elpa"))

(package-initialize)
(unless (file-exists-p package-dir)
  (make-directory package-dir))
(setq package-user-dir package-dir)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	   (package-install package)))
      '(elpy
	rainbow-delimiters
	avy
	exec-path-from-shell
	go-eldoc
	paredit
	magit
	auctex
	undo-tree
	flycheck
	flycheck-color-mode-line
	slime
	paradox
        rust-mode
        projectile
        lsp-mode
        lsp-ui
        lsp-ivy
        lsp-python-ms
        dap-mode
        smartparens
        prescient
        ivy-prescient
        crux
        easy-kill
        dired-sidebar
        ein))

;;load each folder in the elpa directory
(let ((default-directory package-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Font and frame size
(set-face-font 'default "Fira Code Light 10")
(setq default-frame-alist
      (append (list '(width  . 72) '(height . 40)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 10")
                    )))

;(load "~/.emacs.d/elegance")
;(load "~/.emacs.d/sanity")
;x;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;; defuns 
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-nregion (region-beginning) (region-end))))


;;;; global key bindings
;;correctly indent a function definition
(global-set-key (kbd "C-M-z") 'indent-defun)

;;;; rainbow-delimiters
(after "rainbow-delimiters-autoloads" 
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode t))

;;;; prog minor modes
(add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
(electric-pair-mode)
;;;; show-paren-mode
(setq show-paren-style 'parenthesis)
(add-hook 'prog-mode-hook 'show-paren-mode)

;;;; Paredit Mode
(after "paredit-autoloads"
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode))

;;;;Auctex Mode
(after "auctex-autoloads"
  (require 'tex))

;;;; Flycheck Mode
(after "flycheck-autoloads"
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
  (setq flycheck-clang-language-standard "c++11")
  (after "flycheck-color-mode-line-autoloads"
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))


(after "counsel-autoloads"
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (after "ivy-prescient-autoloads"
    (ivy-prescient-mode))
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view))

;;;; in mac add shell path to emacs exec path
(after "exec-path-from-shell-autoloads"
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))
;;;; RUST MODE
(after "rust-mode-autoloads"
  (add-hook 'rust-mode-hook 'rust-mode)
  (after "lsp-mode-autoloads"
    (add-hook 'rust-mode-hook 'lsp))
  (after "lsp-ui-mode-autoloads"
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(after "company-autoloads"
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  ;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

;;;;Org-mode
(after "org-autoloads"
  (define-key global-map (kbd  "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-log-done t))

;;;;Yasnippet
(after "yasnippet-autoloads"
 (add-hook 'prog-mode-hook (lambda () (yas-minor-mode 1))))

;;;;Undo Tree Mode
(after "undo-tree-autoloads"
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

;;; Avy Mode
(after "avy-autoloads"
  (global-set-key (kbd "C-;") 'avy-goto-word-or-subword-1))

(after "lsp-mode-autoloads"
  (after "lsp-ui-mode-autoloads"
    (add-hook 'lsp-mode-hook 'lsp-ui-mode 'lsp-ivy))
  (after "lsp-ivy-autoloads"
    (add-hook 'lsp-mode (lambda () (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol))))
  (after "dap-mode-autoloads"
    (require 'dap-python)
    (add-hook 'lsp-mode-hook #'dap-auto-configure-mode))
  (add-hook 'python-mode-hook #'lsp-deferred)
  (require 'lsp-python-ms)
      
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)))
;; 1mb

(after "dart-mode-autoloads"
  (setq dart-format-on-save t)
  (setq dart-enable-analysis-server t)
  (add-hook 'dart-mode-hook 'flycheck-mode))

;;; ace-window
(after "ace-window-autoloads"
  (global-set-key (kbd "M-o") 'ace-window))

;;; paradox
(after "paradox-autoloads"
  (paradox-enable))

;;; ein

(after "ein-autoloads"
  (require 'ein)
  (require 'ein-notebook))

;;; smartparens
(after "smartparens-autoloads"
  (require 'smartparens-config)
  (add-hook 'python-mode-hook #'smartparens-strict-mode))

(after "dired-sidebar-autoloads"
  (global-set-key (kbd "C-x C-n") #'dired-sidebar-toggle-sidebar))

(after "crux-autoloads"
  (require 'crux)
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key (kbd "s-r") #'crux-recentf-find-file)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bffb799032a7404b33e431e6a1c46dc0ca62f54fdd20744a35a57c3f78586646" "21fb497b14820147b2b214e640b3c5ee19fcadc15bc288e3c16c9c9575d95d66" "2642a1b7f53b9bb34c7f1e032d2098c852811ec2881eec2dc8cc07be004e45a0" "76dc63684249227d64634c8f62326f3d40cdc60039c2064174a7e7a7a88b1587" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "78496062ff095da640c6bb59711973c7c66f392e3ac0127e611221d541850de2" "6a23db7bccf6288fd7c80475dc35804c73f9c9769ad527306d2e0eada1f8b466" "6dd2b995238b4943431af56c5c9c0c825258c2de87b6c936ee88d6bb1e577cb9" "c620ce43a0b430dcc1b06850e0a84df4ae5141d698d71e17de85e7494377fd81" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "8e5dd88c42089566d5f8e1a23d3017c213eeccd94a7b9e1a58a2dc3e08cb26d5" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" default))
 '(ein:jupyter-default-server-command "")
 '(ein:output-area-inlined-images t)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-autodoc elpy-module-sane-defaults))
 '(elpy-project-ignored-directories
   '(".tox" "build" "dist" ".cask" ".ipynb_checkpoints" "test"))
 '(elpy-rpc-virtualenv-path 'current)
 '(fci-rule-color "#3E4451")
 '(flycheck-javascript-eslint-executable nil)
 '(geiser-guile-binary "/usr/local/stow/guile-3/bin/guile")
 '(geiser-guile-load-path
   '("/usr/local/stow/guile-3/share/guile/" "/usr/local/stow/guile-3/share/guile/3.0"))
 '(global-display-line-numbers-mode t)
 '(indent-tabs-mode nil)
 '(lsp-python-ms-extra-paths ["src"])
 '(lsp-ui-flycheck-enable t)
 '(package-selected-packages
   '(dired-sidebar use-package easy-kill crux ivy-prescient prescient smartparens dap-mode lsp-ivy lsp-python-ms solarized-theme ivy projectile counsel ein org ess-R-data-view ess haskell-mode lsp-mode lsp-ui rust-mode dash-alfred bicycle ace-window flycheck-yamllint yaml-mode geiser emojify tuareg flymake-jslint wc-mode ini-mode json-mode ace-jump-mode elpy atom-one-dark-theme markdown-mode go-eldoc powerline go-mode avy atom-dark-theme moe-theme paradox slime exec-path-from-shell flycheck-color-mode-line undo-tree auctex magit paredit hungry-delete flycheck-rust rainbow-delimiters))
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(paradox-spinner-type 'moon)
 '(python-shell-interpreter "jupyter")
 '(python-shell-interpreter-args "\"console --simple-prompt\"")
 '(python-shell-prompt-detect-failure-warning nil)
 '(safe-local-variable-values
   '((elpy-project-root . "\\./")
     (elpy-project-root . \./)
     (elpy-project-root "./")
     (pyvenv-workon "remit")))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
;;  (after "atom-dark-theme-autoloads"
;;    (require 'atom-dark-theme)
;;    (load-theme 'atom-dark))

;; (after "eink-theme-autoloads"
;;   (load-theme 'eink))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; (after "spacemacs-theme-autoloads"
;;   (load-theme 'spacemacs-dark))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(load-theme 'solarized-light-high-contrast)
