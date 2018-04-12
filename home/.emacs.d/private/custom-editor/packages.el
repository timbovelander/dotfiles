(defconst custom-editor-packages
  '(
     editorconfig
     golden-ratio
     writeroom-mode
     ))

(defun custom-editor/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init
    (progn
      (editorconfig-mode)
      (spacemacs|add-toggle editorconfig
        :mode editorconfig-mode
        :evil-leader "te"))
    :config (spacemacs|diminish editorconfig-mode " ⓔ" " e")))

(defun custom-editor/post-init-golden-ratio ()
  (spacemacs/toggle-golden-ratio-on))

(defun custom-editor/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :init
      (spacemacs|add-toggle writeroom
        :mode writeroom-mode
        :evil-leader "TW")
    :config (spacemacs|diminish writeroom-mode " Ⓦ" " W")))
