(defun spacemacs//setup-vue-mode ()
  "Adjust web-mode to accommodate vue-mode"
  (yas-activate-extra-mode 'js-mode)
  (set (make-local-variable 'company-minimum-prefix-length) 2))
