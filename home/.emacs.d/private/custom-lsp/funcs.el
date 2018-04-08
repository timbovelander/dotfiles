(defun spacemacs//lsp-sync-face ()
  (set-face-attribute 'lsp-ui-peek-list nil
    :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-peek nil
    :background (face-attribute 'hl-line :background nil t))
  (set-face-attribute 'lsp-ui-peek-selection nil
    :background (face-attribute 'highlight :background nil t)
    :foreground (face-attribute 'default :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-filename nil
    :foreground (face-attribute 'font-lock-constant-face
                  :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-highlight nil
    :background (face-attribute 'highlight :background nil t)
    :foreground (face-attribute 'highlight :foreground nil t)
    :distant-foreground (face-attribute 'highlight
                          :foreground nil t))
  (set-face-attribute 'lsp-ui-peek-header nil
    :background (face-attribute 'highlight :background nil t)
    :foreground (face-attribute 'default :foreground nil t))

  (set-face-attribute 'lsp-ui-doc-background nil
    :background (face-attribute 'hl-line :background nil t))

  (set-face-attribute 'lsp-face-highlight-read nil
    :background (face-attribute 'highlight :background nil t)
    :foreground (face-attribute 'highlight :foreground nil t)
    :distant-foreground (face-attribute 'highlight
                          :foreground nil t))
  (set-face-attribute 'lsp-face-highlight-write nil
    :background (face-attribute 'highlight :background nil t)
    :foreground (face-attribute 'highlight :foreground nil t)
    :distant-foreground (face-attribute 'highlight
                          :foreground nil t))
  (set-face-attribute 'lsp-face-highlight-textual nil
    :background (face-attribute 'highlight :background nil t)
    :foreground (face-attribute 'highlight :foreground nil t)
    :distant-foreground (face-attribute 'highlight
                          :foreground nil t))
  )
