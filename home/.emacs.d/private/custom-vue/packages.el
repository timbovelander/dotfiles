(defconst custom-vue-packages
  '(
     add-node-modules-path
     company
     company-lsp
     emmet-mode
     evil-matchit
     flycheck
     lsp-vue
     smartparens
     web-mode
     ))

(defun custom-vue/post-init-add-node-modules-path ()
  (eval-after-load 'web-mode
    '(add-hook 'vue-mode-hook #'add-node-modules-path)))

(defun custom-vue/post-init-company ()
  (spacemacs|add-company-hook vue-mode))

(defun custom-vue/post-init-company-lsp ()
  (push 'company-lsp company-backends-vue-mode))

(defun custom-vue/post-init-emmet-mode ()
  (add-hook 'vue-mode-hook 'emmet-mode))

(defun custom-vue/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'vue-mode
      '((evilmi-simple-get-tag evilmi-simple-jump)
         (evilmi-javascript-get-tag evilmi-javascript-jump)
         (evilmi-html-get-tag evilmi-html-jump)))))

(defun custom-vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'vue-mode))
  (spacemacs/add-flycheck-hook 'vue-mode))

(defun custom-vue/init-lsp-vue ()
  (use-package lsp-vue
    :init (add-hook 'vue-mode-hook #'lsp-vue-enable)
    :config (setq lsp-ui-flycheck-enable nil)))

(defun custom-vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))

(defun custom-vue/post-init-web-mode ()
  (define-derived-mode vue-mode web-mode "vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-hook 'spacemacs//setup-vue-mode))
