(defconst custom-javascript-packages
  '(
     company-lsp
     add-node-modules-path
     js2-mode
     lsp-javascript-typescript
     ))

(defun custom-javascript/post-init-company-lsp ()
  (push 'company-lsp company-backends-js2-mode))

(defun custom-javascript/init-add-node-modules-path ()
  (use-package add-node-modules-path
    :defer t
    :init (add-hook 'js2-mode-hook #'add-node-modules-path)))

(defun custom-javascript/post-init-js2-mode ()
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(defun custom-javascript/init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript
    :init (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
    :config (setq lsp-ui-flycheck-enable nil)))
