(defconst custom-lsp-packages
  '(
    (company-lsp :requires company)
    (flycheck-lsp :requires flycheck :location built-in)
    lsp-mode
    lsp-ui
    ))

(defun custom-lsp/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates t)))

(defun custom-lsp/init-flycheck-lsp ()
  (setq lsp-enable-flycheck nil))

(defun custom-lsp/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (progn
      (spacemacs|hide-lighter lsp-mode))))

(defun custom-lsp/init-lsp-ui ()
  (use-package lsp-ui
    :init (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    :config
    (progn
      (spacemacs//lsp-sync-face)
      (add-hook 'spacemacs-post-theme-change-hook
                #'spacemacs//lsp-sync-face))))
