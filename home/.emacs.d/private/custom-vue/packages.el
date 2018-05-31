(defconst custom-vue-packages
  '(
     add-node-modules-path
     company-lsp
     emmet-mode
     evil-matchit
     flycheck
     (lsp-vue :requires lsp-mode)
     smartparens
     web-mode
     yasnippet
     ))

(defun custom-vue/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(vue-mode-hook)))

(defun custom-vue/post-init-company-lsp ()
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-company))

(defun custom-vue/post-init-emmet-mode ()
  (spacemacs/add-to-hooks 'emmet-mode '(vue-mode-hook)))

(defun custom-vue/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'vue-mode
      '((evilmi-simple-get-tag evilmi-simple-jump)
         (evilmi-javascript-get-tag evilmi-javascript-jump)
         (evilmi-html-get-tag evilmi-html-jump)))))

(defun custom-vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'vue-mode))
  (spacemacs/enable-flycheck 'vue-mode))

(defun custom-vue/init-lsp-vue ()
  (use-package lsp-vue
    :commands lsp-vue-enable
    :defer t))

(defun custom-vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))

(defun custom-vue/post-init-web-mode ()
  (define-derived-mode vue-mode web-mode "vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-backend))

(defun javascript/pre-init-yasnippet ()
  (spacemacs|use-package-add-hook yasnippet
    :post-config
    (yas-activate-extra-mode 'js-mode)))
