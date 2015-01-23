(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun my-js-mode-common-hook()
  (setq
   tab-width 2
   js2-basic-offset 2
   indent-tabs-mode nil)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  )

;; (add-hook 'js-mode-hook 'my-js-mode-common-hook)
(add-hook 'js2-mode-hook 'my-js-mode-common-hook)
