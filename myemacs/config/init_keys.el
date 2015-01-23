;; function keys
;; F2 mark a bookmark, and S-F2, C-F2 moves among these bookmarks
;; (global-set-key [f2] viss-bookmark-toggle)
(global-set-key [f1] 'hs-toggle-hiding)
(global-set-key [f2] 'hs-hide-all)
(global-set-key [S-f2] 'hs-show-all)
(global-set-key [f3] 'next-error)
(global-set-key [f4] 'previous-error)
(global-set-key [f5]
                (defun toggle-ecb ()
                  "toggle ecb windows."
                  (interactive)
                  (if (not (fboundp 'ecb-toggle-ecb-windows))
                      (ecb-activate)
                    (ecb-toggle-ecb-windows)
                    (ecb-toggle-compile-window))
                  ))
(global-set-key [f6] 'gdb-many-windows)
(global-set-key [f7] 'my-compile)
(global-set-key [f8] 'ansi-term)
(global-set-key [f9] 'reload-configures)
(global-set-key [f11] 'ff-find-other-file)
(global-set-key [S-f11] 'ff-find-related-file)
(global-set-key [f12] 'gtags-find-tag)
;; (global-set-key [f12] 'semantic-ia-fast-jump) ; go to definition
;; (global-set-key [S-f12] 'def-goback)

;; go back to the last mark
(global-set-key (kbd "C-c b") 'pop-global-mark)

;; it's ok in unity, but not working in windows and gnome3
;(global-set-key [f11] 'toggle-fullscreen)

;; gtags keys
(global-set-key (kbd "C-c s") 'gtags-find-tag)
(global-set-key (kbd "C-c q") 'gtags-find-rtag)
(global-set-key (kbd "C-c p") 'gtags-find-pattern)

;; gdb key bindings
(add-hook 'gdb-mode-hook
          (lambda ()
            (message "setting up keys for debug mode")
            (define-key c-mode-base-map [(f5)] 'gud-go)
            (define-key c-mode-base-map [(f9)] 'gud-break)
            (define-key c-mode-base-map [(f11)] 'gud-step)
            (define-key c-mode-base-map [S-f11] 'gud-next)))

;; open a terminal
(global-set-key (kbd "C-t") 'multi-term)

;; open file as root. this key conflict with ido's find-find-readonly
(global-set-key [(control x) (control r)] 'find-file-root)

;; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; C-m is originally set for newline-and-indent
(global-set-key (kbd "M-m") 'set-mark-command)


(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; tabbar buffer switching
(global-set-key (kbd "C--") 'tabbar-backward-group)
(global-set-key (kbd "C-=") 'tabbar-forward-group)
(global-set-key (kbd "C-9") 'tabbar-backward)
(global-set-key (kbd "C-0") 'tabbar-forward)

;; use ibuffer
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

;; revert buffer from file
(global-set-key (kbd "C-c r") 'revert-buffer)

;; find charactor in current line
(define-key global-map (kbd "C-c f") 'wy-go-to-char)

;; Kill entire line
(global-set-key (kbd "C-c k") 'kill-whole-line)

;; Delete dumplicated space
(define-key global-map (kbd "C-c d") 'just-one-space)

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Remove ^M
(global-set-key "\C-cm" (lambda()
                          (interactive)
                          (save-excursion
                            (move-to-window-line 0)
                            (while (re-search-forward "" nil t)
                              (replace-match "")))))
;; Go to line
(define-key global-map (kbd "C-c g") 'goto-line)

;; Windows Cycling
(defun windmove-up-cycle()
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down)
             (error (condition-case nil (windmove-right)
                      (error (condition-case nil (windmove-left)
                               (error (windmove-up))))))))))

(defun windmove-down-cycle()
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up)
             (error (condition-case nil (windmove-left)
                      (error (condition-case nil (windmove-right)
                               (error (windmove-down))))))))))

(defun windmove-right-cycle()
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left)
             (error (condition-case nil (windmove-up)
                      (error (condition-case nil (windmove-down)
                               (error (windmove-right))))))))))

(defun windmove-left-cycle()
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right)
             (error (condition-case nil (windmove-down)
                      (error (condition-case nil (windmove-up)
                               (error (windmove-left))))))))))

;; keys like M-<up> doesn't work in ssh and org mode
(global-set-key (kbd "C-x <up>") 'windmove-up-cycle)
(global-set-key (kbd "C-x <down>") 'windmove-down-cycle)
(global-set-key (kbd "C-x <right>") 'windmove-right-cycle)
(global-set-key (kbd "C-x <left>") 'windmove-left-cycle)
(global-set-key (kbd "M-<up>") 'windmove-up-cycle)
(global-set-key (kbd "M-<down>") 'windmove-down-cycle)
(global-set-key (kbd "M-<right>") 'windmove-right-cycle)
(global-set-key (kbd "M-<left>") 'windmove-left-cycle)

(define-key global-map (kbd "C-{") 'shrink-window-horizontally)
(define-key global-map (kbd "C-}") 'enlarge-window-horizontally)

;; resize fonts with ctrl-wheel
(defun font-big ()
 (interactive)
 (set-face-attribute 'default nil :height
  (+ (face-attribute 'default :height) 10)))

(defun font-small ()
 (interactive)
 (set-face-attribute 'default nil :height
  (- (face-attribute 'default :height) 10)))

(global-set-key [C-wheel-up] 'font-small)
(global-set-key [C-wheel-down] 'font-big)
(global-set-key [C-mouse-4] 'font-small)
(global-set-key [C-mouse-5] 'font-big)

(global-set-key (kbd "C-c >") 'python-shift-right)
(global-set-key (kbd "C-c <") 'python-shift-left)

;; helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
