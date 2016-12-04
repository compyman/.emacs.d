;; init.el --- Milkmacs configuration file
;;;; Emacs Settings
;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))

;;set backup behavior
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; No splash screen please...
(setq inhibit-startup-screen t)

;;;; package.el
(package-initialize)
(unless (file-exists-p "~/.emacs.d/elpa/")
  (make-directory "~/.emacs.d/elpa"))
(setq package-user-dir "~/.emacs.d/elpa/")
		
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives
;;	     '("gnu" . "http://elpa.gnu.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))
(mapc #'(lambda (package)
	 (unless (package-installed-p package)
	   (package-install package)))
      '(rainbow-delimiters
	avy
	company
	rust-mode
	racer
	company-racer
	company-go
	go-eldoc
	flycheck-rust
	hungry-delete
	paredit
	magit
	js2-mode
	ac-js2
	auctex
	undo-tree
	flycheck
	flycheck-color-mode-line
	slime
	paradox
	moe-theme))

;;load each folder in the elpa directory
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;; macros
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
    (indent-region (region-beginning) (region-end))))


;;;; global key bindings
;;correctly indent a function definition
(global-set-key (kbd "C-M-z") 'indent-defun)

;;;; rainbow-delimiters
(after "rainbow-delimiters-autoloads" 
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode t))

;;;; prog minor modes
(add-hook 'prog-mode-hook 'global-linum-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

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
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (setq flycheck-clang-language-standard "c++11")
  (after "flycheck-color-mode-line-autoloads"
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;;;; in mac add shell path to emacs exec path
(after "exec-path-from-shell-autoloads"
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))
;;;; RUST MODE
(after "rust-mode-autoloads"
  (after "racer-autoloads"
    (setq racer-cmd "~/builds/racer/target/release/racer")
    (setq racer-rust-src-path "~/builds/rust/src")
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'rust-mode-hook 'eldoc-mode))
  (after "flycheck-autoloads"
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (set (make-local-variable 'company-backends)'(company-racer))
  (local-set-key (kbd "M-.") 'racer-find-definition)) 

(after "company-autoloads"
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))
;;;; Go Mode
(after "go-mode-autoloads"
  (after "go-eldoc-autoloads"
    (add-hook 'go-mode-hook 'go-eldoc-setup))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list 'company-backends 'company-go))

;;;;Org-mode
(after "org-autoloads"
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))

;;;;Javascript Mode
(after "js2-mode-autoloads"
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js-mode-hook 'js2-mode)
  (setq js2-highlight-level 3)
  (after "ac-js2-autoloads"
     (add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
    (add-hook 'js-mode-hook 'ac-js2-mode))
  (after "paredit-autoloads"
    (after "js"
      (define-key js-mode-map "{" 'paredit-open-curly)
      (define-key js-mode-map "}" 'paredit-close-curly-and-newline))))

;;;;Yasnippet
(after "yasnippet-autoloads"
 (add-hook 'prog-mode-hook (lambda () (yas-minor-mode 1))))


;;;;Hungry Delete Mode
;; MOST IMPORTANT
;; (after "hungry-delete-autoloads"
;;   (global-hungry-delete-mode))
;;;;Undo Tree Mode
(after "undo-tree-autoloads"
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

;;; Avy Mode
(after "avy-autoloads"
  (global-set-key (kbd "C-;") 'avy-goto-char))


;;; init.el ends here





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "32e3693cd7610599c59997fee36a68e7dd34f21db312a13ff8c7e738675b6dfc" "8e5dd88c42089566d5f8e1a23d3017c213eeccd94a7b9e1a58a2dc3e08cb26d5" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" default)))
 '(package-selected-packages
   (quote
    (company-go go-eldoc company-racer company powerline go-mode avy atom-dark-theme moe-theme paradox slime exec-path-from-shell flycheck-color-mode-line undo-tree auctex ac-js2 js2-mode magit paredit hungry-delete flycheck-rust racer rust-mode rainbow-delimiters)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(after "atom-dark-theme-autoloads"
  (require 'atom-dark-theme)
  (load-theme 'atom-dark))

