(ray-add-path "plugins/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
        (concat
         config-base-path
         "plugins/auto-complete-1.3.1/dict"))
(ac-config-default)

(ac-fuzzy-complete)


;; golang
(require 'go-autocomplete)
(require 'auto-complete-config)

;; case insensitiviy
(setq ac-ignore-case t)

(setq ac-trigger-commands
      '(self-insert-command
        autopair-insert-or-skip-quote))

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-files-in-current-dir
               ac-source-filename))

;; add clang autocomplete function
(require 'auto-complete-clang)

(defun ac-settings-4-c ()
  (add-to-list 'ac-omni-completion-sources
               (cons "\\." '(ac-source-semantic)))
  (add-to-list 'ac-omni-completion-sources
               (cons "->" '(ac-source-semantic)))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-clang
          ac-source-semantic
          ac-source-functions
          ac-source-variables
          ac-source-words-in-buffer
          ac-source-files-in-current-dir
          ac-source-filename
          ))
  )

(defun ac-settings-4-lisp ()
  (setq ac-omni-completion-sources
        '(("require\s+'" ac-source-emacs-lisp-features)
          ("load\s+\"" ac-source-emacs-lisp-features)))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-dictionary
          ac-source-symbols
          ac-source-functions
          ac-source-variables
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-files-in-current-dir
          ac-source-filename))
  )

(defun ac-settings-4-js ()
  (setq ac-sources
        '(ac-source-dictionary
          ac-source-yasnippet
          ac-source-words-in-buffer
          ac-source-filename
          ac-source-files-in-current-dir
          ))
  )

(add-hook 'c-mode-hook 'ac-settings-4-c)
(add-hook 'c++-mode-hook 'ac-settings-4-c)
(add-hook 'emacs-lisp-mode-hook 'ac-settings-4-lisp)
(add-hook 'javascript-mode-hook 'ac-settings-4-js)
