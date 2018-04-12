(global-set-key (kbd "<home>") 'beginning-of-line-text)
(global-set-key (kbd "<end>") 'end-of-line)

(define-key evil-normal-state-map (kbd "RET") (lambda() (interactive)(evil-open-below 1)(evil-force-normal-state)))
(define-key evil-normal-state-map (kbd "S-<return>") (lambda() (interactive)(evil-open-above 1)(evil-force-normal-state)))

(define-key evil-insert-state-map (kbd "C-<return>") (lambda() (interactive)(evil-force-normal-state)(evil-open-below 1)))
(define-key evil-insert-state-map (kbd "C-S-<return>") (lambda() (interactive)(evil-force-normal-state)(evil-open-above 1)))
