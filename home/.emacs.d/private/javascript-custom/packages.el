(setq javascript-custom-packages
  '(
     add-node-modules-path
     flycheck
     rjsx-mode
     tide))

(defun javascript-custom/init-add-node-modules-path ()
  (use-package add-node-modules-path
    :defer t
    :init
    (progn
      (eval-after-load 'js2-mode
        '(add-hook 'js2-mode-hook #'add-node-modules-path)))))

(defun javascript-custom/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'rjsx-mode))

(defun javascript-custom/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t))

(defun javascript-custom/post-init-tide ()
  (use-package tide
    :defer t
    :init
    (progn
      (evilified-state-evilify tide-references-mode tide-references-mode-map
        (kbd "C-k") 'tide-find-previous-reference
        (kbd "C-j") 'tide-find-next-reference
        (kbd "C-l") 'tide-goto-reference)
      (add-hook 'js2-mode-hook 'tide-setup)
      (push 'company-tide company-backends-js2-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "gt" 'tide-jump-to-definition
        "gb" 'tide-jump-back
        "gu" 'tide-references
        "hh" 'tide-documentation-at-point
        "rrs" 'tide-rename-symbol))))
