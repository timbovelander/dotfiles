;;; packages.el --- spacemacs-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Tim Bovelander <tim@timenwietske.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs-ui-packages
  '(
     golden-ratio
     info+
     smooth-scrolling
     spaceline
     ))

(defun spacemacs-ui/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (spacemacs|add-toggle golden-ratio
      :status golden-ratio-mode
      :on (golden-ratio-mode) (golden-ratio)
      :off (golden-ratio-mode -1) (balance-windows)
      :documentation "Resize the focused window using the golden ratio."
      :evil-leader "tg")
    :config
    (progn
      (setq golden-ratio-exclude-modes '("bs-mode"
                                         "calc-mode"
                                         "ediff-mode"
                                         "dired-mode"
                                         "gud-mode"
                                         "gdb-locals-mode"
                                         "gdb-registers-mode"
                                         "gdb-breakpoints-mode"
                                         "gdb-threads-mode"
                                         "gdb-frames-mode"
                                         "gdb-inferior-io-mode"
                                         "gud-mode"
                                         "gdb-inferior-io-mode"
                                         "gdb-disassembly-mode"
                                         "gdb-memory-mode"
                                         "restclient-mode"
                                         "speedbar-mode"
                                         ))

      (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

      (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(ace-window
                      ace-delete-window
                      ace-select-window
                      ace-swap-window
                      ace-maximize-window
                      avy-pop-mark
                      evil-avy-goto-word-or-subword-1
                      evil-avy-goto-line
                      windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
                      evil-window-delete
                      evil-window-split
                      evil-window-vsplit
                      evil-window-left
                      evil-window-right
                      evil-window-up
                      evil-window-down
                      evil-window-bottom-right
                      evil-window-top-left
                      evil-window-mru
                      evil-window-next
                      evil-window-prev
                      evil-window-new
                      evil-window-vnew
                      evil-window-rotate-upwards
                      evil-window-rotate-downwards
                      evil-window-move-very-top
                      evil-window-move-far-left
                      evil-window-move-far-right
                      evil-window-move-very-bottom
                      select-window-0
                      select-window-1
                      select-window-2
                      select-window-3
                      select-window-4
                      select-window-5
                      select-window-6
                      select-window-7
                      select-window-8
                      select-window-9
                      buf-move-left
                      buf-move-right
                      buf-move-up
                      buf-move-down
                      ess-eval-buffer-and-go
                      ess-eval-function-and-go
                      ess-eval-line-and-go)))

      ;; Disable auto-resizing for some buffers
      (defun spacemacs/no-golden-ratio-for-buffers (bufname)
        "Disable golden-ratio if BUFNAME is the name of a visible buffer."
        (and (get-buffer bufname) (get-buffer-window bufname 'visible)))
      (defun spacemacs/no-golden-ratio-guide-key ()
        "Disable golden-ratio for guide-key popwin buffer."
        (or (spacemacs/no-golden-ratio-for-buffers " *guide-key*")
            (spacemacs/no-golden-ratio-for-buffers " *popwin-dummy*")))
      (add-to-list 'golden-ratio-inhibit-functions
                   'spacemacs/no-golden-ratio-guide-key)
      (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
      (add-to-list 'golden-ratio-exclude-buffer-names "*LV*")
      (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")

      (spacemacs|diminish golden-ratio-mode " ⓖ" " g"))))

(defun spacemacs-ui/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (with-eval-after-load 'info
        (require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs-ui/init-smooth-scrolling ()
  (use-package smooth-scrolling
    :init
    (progn
      (setq smooth-scroll-margin 5)
      (spacemacs|add-toggle smooth-scrolling
        :status smooth-scrolling-mode
        :on (progn
              (smooth-scrolling-mode)
              (enable-smooth-scroll-for-function previous-line)
              (enable-smooth-scroll-for-function next-line)
              (enable-smooth-scroll-for-function isearch-repeat))
        :off (progn
               (smooth-scrolling-mode -1)
               (disable-smooth-scroll-for-function previous-line)
               (disable-smooth-scroll-for-function next-line)
               (disable-smooth-scroll-for-function isearch-repeat))
        :documentation "Smooth scrolling."
        :evil-leader "tv")
      (when dotspacemacs-smooth-scrolling
        (spacemacs/toggle-smooth-scrolling-on))
      ;; add hooks here only for emacs built-in packages that are not owned
      ;; by a layer.
      (defun spacemacs//unset-scroll-margin ()
        "Set scroll-margin to zero."
        (setq-local scroll-margin 0))
      (spacemacs/add-to-hooks 'spacemacs//unset-scroll-margin
                              '(messages-buffer-mode-hook)))))

(defun spacemacs-ui/init-spaceline ()
  (use-package spaceline-config
    :init
    (progn
      (spacemacs|do-after-display-system-init
       (setq-default powerline-default-separator
                     (if (display-graphic-p) 'wave 'utf-8)))
      (defun spacemacs//set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
          (when (and (get-buffer buffer)
                     (configuration-layer/package-usedp 'spaceline))
            (spacemacs//restore-powerline buffer))))
      (add-hook 'emacs-startup-hook
                'spacemacs//set-powerline-for-startup-buffers))
    :config
    (progn
      (defun spacemacs/customize-powerline-faces ()
        "Alter powerline face to make them work with more themes."
        (set-face-attribute 'powerline-inactive2 nil
                            :inherit 'font-lock-comment-face))
      (spacemacs/customize-powerline-faces)

      (dolist (spec '((minor-modes "tmm")
                      (major-mode "tmM")
                      (version-control "tmv")
                      (new-version "tmV")
                      (point-position "tmp")
                      (org-clock "tmc")))
        (let* ((segment (car spec))
               (status-var (intern (format "spaceline-%S-p" segment))))
          (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
                   :status ,status-var
                   :on (setq ,status-var t)
                   :off (setq ,status-var nil)
                   :documentation ,(format "Show %s in the mode-line."
                                           (replace-regexp-in-string
                                            "-" " " (format "%S" segment)))
                   :evil-leader ,(cadr spec)))))
      (setq spaceline-org-clock-p nil)

      (defun spacemacs//evil-state-face ()
        (if (bound-and-true-p evil-state)
            (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
              (intern (format "spacemacs-%S-face" state)))
          'face-of-god))
      (setq spaceline-highlight-face-func 'spacemacs//evil-state-face)

      (let ((unicodep (dotspacemacs|symbol-value
                       dotspacemacs-mode-line-unicode-symbols)))
        (setq spaceline-window-numbers-unicode unicodep)
        (setq spaceline-workspace-numbers-unicode unicodep))

      (defpowerline spacemacs-powerline-new-version
        (propertize
         spacemacs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update"
                            spacemacs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event)
                          (interactive "@e")
                          (if (yes-or-no-p
                               (format (concat "Do you want to update to the newest "
                                               "version %s ?") spacemacs-new-version))
                              (progn
                                (spacemacs/switch-to-version spacemacs-new-version))
                            (message "Update aborted."))))
                      map)))

      (spaceline-define-segment new-version
        (spacemacs-powerline-new-version
         (spacemacs/get-new-version-lighter-face
          spacemacs-version spacemacs-new-version))
        :when spacemacs-new-version)

      (spaceline-spacemacs-theme '(new-version :when active))
      (spaceline-helm-mode t)
      (when (configuration-layer/package-usedp 'info+)
        (spaceline-info-mode t))

      (defun spacemacs//restore-powerline (buffer)
        "Restore the powerline in buffer"
        (with-current-buffer buffer
          (setq-local mode-line-format (default-value 'mode-line-format))
          (powerline-set-selected-window)
          (powerline-reset)))

      (defun spacemacs//prepare-diminish ()
        (when spaceline-minor-modes-p
          (let ((unicodep (dotspacemacs|symbol-value
                           dotspacemacs-mode-line-unicode-symbols)))
            (setq spaceline-minor-modes-separator
                  (if unicodep (if (display-graphic-p) "" " ") "|"))
            (dolist (mm spacemacs--diminished-minor-modes)
              (let ((mode (car mm)))
                (when (and (boundp mode) (symbol-value mode))
                  (let* ((unicode (cadr mm))
                         (ascii (caddr mm))
                         (dim (if unicodep
                                  unicode
                                (if ascii ascii unicode))))
                    (diminish mode dim))))))))
      (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish))))
