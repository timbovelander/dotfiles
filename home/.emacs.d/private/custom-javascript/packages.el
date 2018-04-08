(setq custom-javascript-packages
  '(
     company-lsp
     add-node-modules-path
     lsp-javascript-typescript))

(defun custom-vue/post-init-company-lsp ()
  (push 'company-lsp company-backends-js2-mode))

(defun custom-javascript/init-add-node-modules-path ()
  (use-package add-node-modules-path
    :defer t
    :init
    (progn
      (eval-after-load 'js2-mode
        '(add-hook 'js2-mode #'add-node-modules-path)))))

(defun custom-javascript/init-lsp-javascript-typescript ()
  (use-package lsp-javascript-typescript
    :init (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
    :config (setq lsp-ui-flycheck-enable nil)))
