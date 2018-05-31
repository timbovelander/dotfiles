(defun spacemacs//vue-setup-backend ()
  (spacemacs//setup-lsp-jump-handler 'vue-mode)
  (lsp-vue-enable))

(defun spacemacs//vue-setup-company ()
  (fix-lsp-company-prefix)
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes vue-mode
    :variables company-minimum-prefix-length 2
    :append-hooks nil
    :call-hooks t)
  (company-mode))
