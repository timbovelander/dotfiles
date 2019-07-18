(defun spacemacs//vue-setup-backend ()
  "Setup lsp backend."
  (setq-local lsp-prefer-flymake :none)
  (spacemacs//setup-lsp-jump-handler 'vue-mode)
  (lsp))

(defun spacemacs//vue-setup-company ()
  "Setup lsp auto-completion."
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes vue-mode
    :variables company-minimum-prefix-length 2
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun spacemacs//vue-setup-yasnippet ()
  (yas-activate-extra-mode 'js-mode))
