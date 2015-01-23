;; Personal Info
(setq user-mail-address "ranjiao@gmail.com")
(setq user-full-name "Raymond")

;; check emacs version
(defconst is-before-emacs-21
  (>= 21 emacs-major-version) "emacs 21 or earlier verion")
(defconst is-after-emacs-23
  (<= 23 emacs-major-version) "emacs 23 or later version")

;; check current os
(defconst is-windows
  (string-equal system-type "windows-nt") "any kinds of windows")
(defconst is-linux
  (string-equal system-type "gnu/linux") "any kinds of windows")
(defconst is-mac
  (string-equal system-type "darwin") "any kinds of mac os")

;; check console
(defconst is-console (not (display-graphic-p)) "are we in console?")

(if is-windows
    (progn (setenv "HOME" "D:/")
           (setenv "PATH" "D:/")
           (message "Setting up windows paths")))

;; set load path
(setq config-base-path "~/proj/work/raysconfig/myemacs/")
(defun ray-add-path (path)
  (interactive)
  (add-to-list 'load-path
               (concat config-base-path path)))
(defun ray-load-file (file)
  (interactive)
  (load-file (concat config-base-path file)))

(ray-add-path "config")
(ray-add-path "plugins")

(load-library "init_misc.el")
(load-library "init_develop.el")
(load-library "init_cpp.el")
(load-library "init_latex.el")
(load-library "init_cmake.el")
(load-library "init_js.el")
;; (load-library "init_html.el")
(load-library "init_python.el")
(load-library "init_erc.el")
(load-library "init_auto-complete.el")
(load-library "init_keys.el")
(load-library "init_appearance.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "color-21" :underline t)))))
