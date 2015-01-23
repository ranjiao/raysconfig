;; automaticlly break a long function declaration into lines
(defconst my-c-lineup-maximum-indent 30)
(defun my-c-lineup-arglist (langelem)
  (let ((ret (c-lineup-arglist langelem)))
    (if (< (elt ret 0) my-c-lineup-maximum-indent)
        ret
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) 8))))))
(defun my-indent-setup ()
  (setcdr (assoc 'arglist-cont-nonempty c-offsets-alist)
          '(c-lineup-gcc-asm-reg my-c-lineup-arglist)))

;; open headers in c++ mode
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)) auto-mode-alist))


(defun my-c-mode-common-hook()
  (message "running c mode common hook")
  (c-set-offset 'brace-list-open 0)

  ;;use different c/c++ style for work and personal projects
  (if is-at-office
      (setq
       c-default-style "bsd"
       c-basic-offset 4 ;; indent 4 spaces
       default-tab-width 4
       tab-width 4 ;; a tab equas 4 spaces
       indent-tabs-mode nil ;; insert spaces other than tabs
       )
    (setq
     c-default-style "bsd"
     c-basic-offset 2 ;; indent 4 spaces
     default-tab-width 2
     tab-width 2 ;; a tab equas 4 spaces
     indent-tabs-mode nil ;; insert spaces other than tabs
     )
    )

  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'template-args-cont 1) ;; don't indent on substatement

  ;; hungry-delete and disable auto-newline
  (c-toggle-auto-hungry-state 1)
  (c-toggle-auto-newline 1)

  (define-key c-mode-base-map [(control \')] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  ;; (define-key c-mode-base-map [M-f12] 'ff-find-other-file)
  (define-key c-mode-base-map (kbd "C-c h") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "C-x C-b") 'bs-show)
  (define-key c-mode-base-map [f1] 'hs-toggle-hiding)
  (hs-minor-mode)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

;; gtags
(load-library "gtags")

;; use gtags in c mode
(setq c-mode-hook
      '(lambda ()
         (gtags-mode 1)
         ))
;; auto update gtags. too slow for large projects
;; (defun gtags-root-dir ()
;;   "Returns GTAGS root directory or nil if doesn't exist."
;;   (with-temp-buffer
;;     (if (zerop (call-process "global" nil t nil "-pr"))
;;         (buffer-substring (point-min) (1- (point-max)))
;;       nil)))
;; (defun gtags-update ()
;;   "Make GTAGS incremental update"
;;   (call-process "global" nil nil nil "-u"))
;; (defun gtags-update-hook ()
;;   (when (gtags-root-dir)
;;     (gtags-update)))
;; (add-hook 'after-save-hook #'gtags-update-hook)
