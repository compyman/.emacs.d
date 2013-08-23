(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.03)
 '(ac-modes (quote (emacs-lisp-mode lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode ts-mode scheme-mode)))
 '(company-idle-delay 0.3)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(custom-theme-load-path (quote (custom-theme-directory t)))
 '(geiser-mode-company-p t)
 '(geiser-mode-smart-tab-p t)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; init.el --- Milkmacs configuration file
;;;; Emacs Settings
;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;set backup behavior
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;;;; package.el
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;;load each folder in the elpa directory
(let ((default-directory "~/.emacs.d/elpa/"))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "/usr/lib/chicken/6/")   ; Where Eggs are installed
;(load-file "~/.emacs.d/rainbow-delimiters.elc")

;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))
;;;; defun 
(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))
;;;; global key bindings

;;correctly indent a function definition
(global-set-key (kbd "C-M-z") 'indent-defun)

;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
;;;; rainbow-delimiters
(after 'rainbow-delimiters-autoloads 
 (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode t)
 (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode t))
;;;; prog minor modes
(add-hook 'prog-mode-hook 'global-linum-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

;;;; show-paren-mode
(after 'show-paren-mode
  (setq show-paren-style 'parenthesis)
  (add-hook 'prog-mode-hook 'show-paren-mode))

;;;; Geiser mode
;;Geiser doesn't really need configuration (I think)

;;;; Company Mode
(after 'company-mode
  (add-hook 'scheme-mode company-mode))

;;;; themes
;;load each theme in the themes folder
(mapc 
 (lambda (x) (add-to-list 'custom-theme-load-path (concat "~/.emacs.d/themes/" x)))
 (directory-files "~/.emacs.d/themes/" 'nil "^.+"))
(load-theme 'solarized-dark)
;;; init.el ends here


