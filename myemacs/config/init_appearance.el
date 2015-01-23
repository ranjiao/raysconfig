;; High light current line
;; (require 'hl-line)
;; (global-hl-line-mode t)
(if (not is-console)
    (progn
      ;; hightlight tail
      ;;(require 'highlight-tail)
      ;;(highlight-tail-mode)
      ))

;; get rid of ugly background in terminal
(if is-console
    (progn
      (setq term-default-bg-color nil)
      (setq term-default-fg-color "#cccccc"))
  (setq term-default-bg-color nil)
  (setq term-default-fg-color "#111111"))

;; setup theme
(ray-add-path "plugins/color-theme-6.6.0/")
(require 'color-theme)
;; (require 'color-theme-ahei)
(eval-after-load 'color-theme
  '(progn
     (color-theme-initialize)
     (if is-console
         (color-theme-dark-laptop)
       (color-theme-rotor)
       ;; (color-theme-jsc-light2)
     )))

;; set global font size
;(set-face-attribute 'default nil :height 90)

(defun my-console-buffer-face-mode()
  (buffer-face-mode)
  (buffer-face-set 'my-console-face)
  )
(dolist (hook '(term-mode-hook
                ))
  (add-hook hook (lambda () (my-console-buffer-face-mode))))

;; get rid of tool bar
(tool-bar-mode -1)

;; setup different fonts for english and chinese
(if (and is-linux (not is-console))
    (progn
      ;; setup special font for terminal buffers
      (make-face 'my-console-face)
      ;; (copy-face 'default my-console-face)
      (set-face-attribute 'my-console-face
                          nil
                          :font "DejaVu Sans Mono-9")

      ;; (set-frame-font "Bitstream Vera Sans Mono-10.0")
      (set-frame-font "DejaVu Sans Mono-9.0")
      ;; (set-frame-font "WenQuanYi Micro Hei Mono-10")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "WenQuanYi Zen Hei Mono"
                                     :size 14)))
      ))

;; Put scroll bar at right side
(customize-set-variable 'scroll-bar-mode 'right)
