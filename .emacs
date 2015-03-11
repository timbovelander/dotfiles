;; Disable splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Enable CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; Autosave and Backup files in temp dir
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

;; Backup files in other folder and increase number of backups
(setq
  backup-by-copying t ;; don't clobber symlinks
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t) ;; use versioned backups

;; Allways show line numbers
(global-linum-mode t)

;; Load custom color themes and load base16-theme
(add-to-list 'load-path "~/.emacs.d/custom-themes")
(require 'base16-theme)

;; Add editorconfig support to emacs
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "editorconfig")
