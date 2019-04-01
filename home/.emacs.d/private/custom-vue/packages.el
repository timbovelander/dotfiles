(defconst custom-vue-packages
  '(
     add-node-modules-path
     company
     emmet-mode
     evil-matchit
     flycheck
     smartparens
     web-mode
     yasnippet
     ))

(defun custom-vue/post-init-add-node-modules-path ()
  (add-hook 'vue-mode-hook #'add-node-modules-path))

(defun custom-vue/post-init-company ()
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-company))

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
  (spacemacs/enable-flycheck 'vue-mode))


(defun custom-vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
    (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))

(defun custom-vue/post-init-web-mode ()
  (define-derived-mode vue-mode web-mode "vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-backend))

(defun custom-vue/post-init-yasnippet ()
  (add-hook 'vue-mode-hook #'spacemacs//vue-setup-yasnippet))
