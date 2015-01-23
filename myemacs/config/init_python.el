;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(defun my-python-mode-common-hook()
  (setq tab-width 4
        indent-tabs-mode nil
        abbrev-mode t
        python-indent 4
        )
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (hs-minor-mode)
  )
(add-hook 'python-mode-hook 'my-python-mode-common-hook)
