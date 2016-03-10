; package management
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(setq package-enable-at-startup nil)
(package-initialize)

(setq package-list '( company
                      company-tern
                      editorconfig
                      evil
                      flycheck
                      helm
                      helm-ag
                      helm-projectile
                      gitconfig-mode
                      gitignore-mode
                      js2-mode
                      json-mode
                      magit
                      projectile
                      sass-mode
                      solarized-theme
                      tern
                      web-mode))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

; backups
(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

; autosave
(setq auto-save-default nil)

; appearance
(load-theme 'solarized-dark t)
(add-to-list 'default-frame-alist '(font . "Hack-11"))

; enable/disable global modes
(editorconfig-mode 1)
(electric-pair-mode 1)
(evil-mode 1)
(global-company-mode 1)
(global-flycheck-mode 1)
(global-linum-mode 1)
(menu-bar-mode -1)
(projectile-global-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

; helm
(setq helm-M-x-fuzzy-match t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

; company-mode
(setq company-idle-delay 0)
(add-to-list 'company-backends 'company-tern)

; key bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

; associate file-types
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.template\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.schema\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))

; hooks
(add-hook 'js2-mode-hook (lambda () (tern-mode 1)))

(setq web-mode-engines-alist
  '(
     ("ctemplate" . "\\.template\\'")))
