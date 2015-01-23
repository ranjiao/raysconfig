(require 'package)
(add-to-list 'package-archives
              '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
   ;; For important compatibility libraries like cl-lib
   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this l

;; diable beep
(setq visible-bell 1)

;; indent pasted string
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode
                        '(emacs-lisp-mode
                          lisp-mode
                          clojure-mode
                          scheme-mode
                          haskell-mode
                          ruby-mode
                          rspec-mode
                          python-mode
                          c-mode
                          c++-mode
                          objc-mode
                          latex-mode
                          plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; auto untabify buffer
(defun auto-untabify ()
  "Untabify current buffer"
  (interactive)
  (message "untabifying current buffer")
  (untabify (point-min) (point-max)))
(defun programming-modes-hooks ()
  "Hook untabify for programming modes"
  (add-hook 'before-save-hook 'programming-modes-write-hook))
(defun programming-modes-write-hook ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    ;; maybe i can do more things while saving
    (message "customized wirte hook running")
    (set-buffer-file-coding-system 'utf-8-unix)
    (auto-untabify)))
;; (add-hook 'c++-mode-hook 'programming-modes-hooks)
;; (add-hook 'js2-mode-hook 'programming-modes-hooks)
;; (add-hook 'emacs-lisp-mode 'programming-modes-hooks)
;; (add-hook 'python-mode-hook 'programming-modes-hooks)

;; turn on auto-fill and spell check for org mode
(add-hook 'org-mode-hook
          (lambda ()
            turn-on-auto-fill
            flyspell-mode))

;; restore layout automaticlly
;; (require 'workgroups)
;; (workgroups-mode 1)

;; delete selected region instead of a single character
(delete-selection-mode 1)

;; undo tree
(require 'undo-tree)
(global-undo-tree-mode t)


;; stop saving tramp buffers
;; (add-hook 'desktop-save-hook 'tramp-cleanup-all-buffers)


;; auto revert log files
(add-hook 'find-file-hook
          (lambda ()
            (if
                (and
                 (> (length (buffer-file-name)) 3)
                 (string-equal
                  ".log"
                  (substring (buffer-file-name) -3 nil)))
                ;; auto revert log files
                (progn
                  (setq auto-revert-mode t)
                  (message "Enabling auto-revert-mode for log file")))
            ))

;; keep cursor position after page up and page down
;; (require 'scroll-in-place)
;; can also be done by (scroll-lock-mode t)
;; also we shouldn't need this if scroll-in-place is working
;;(setq scroll-preserve-screen-position t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)

;; deal with white spaces
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style
      '(face trailing lines lines-tail empty
        space-after-tab space-before-tab))
(setq whitespace-line-column 250)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; cg mode
(require 'cg-mode)

;; use ido, used for C-c b and C-x C-f.
(require 'ido)
(ido-mode t)
(setq
 ido-ignore-buffers '("\\` " "^\*compilation" "^\*GTAGS")
 ;; disable the confirmation for new file
 confirm-nonexistent-file-or-buffer nil)

;; reload emacs config
(defun reload-configures()
  "reload emacs configeration."
  (interactive)
  (if is-linux
      (load-file "~/.emacs"))
  (if is-windows
      (load-file (concat config-base-path ".emacs")))
  (message "Reloading emacs config done."))

;; Fullscreen
(defun toggle-fullscreen ()
  "toggles whether the currently selected frame consumes the
entire display or is decorated with a window border"
  (interactive)
  (let ((f (selected-frame)))
    (modify-frame-parameters
     f
     `((fullscreen . ,(if (eq nil (frame-parameter f 'fullscreen))
                          'fullboth
                        nil))))))

;; setting OS specified configures
(if is-windows
    (progn
      (setq cygwin-bin "d:\\software\\mingw\\bin")
      (setq gnu-bin "d:\\software\\mingw\\msys\\1.0\\bin")
      (setenv "PATH"
              (concat cygwin-bin ";" gnu-bin ";"))
      (setq exec-path
            '(cygwin-bin gnu-bin))

      ;; maximize the emacs frame at start
      (w32-send-sys-command 61488)
      ))
(if is-linux
    (progn
      ;; maximize the emacs frame at startup
      ;; (toggle-fullscreen)
      ))

;; Grammar high light
(global-font-lock-mode t)

;; yank at cursor other than mouse pos
(setq mouse-yank-at-point t)

;; filename as window title
(setq frame-title-format "%b")

(setq tab-width 4)
;; use space to indent instead of tab
(setq-default indent-tabs-mode nil)
(setq tab-stop-list nil)
(loop for x downfrom 20 to 1 do
      (setq tab-stop-list (cons (* x tab-width) tab-stop-list)))

;; Copy and paste with other applications
(setq x-select-enable-clipboard t)

;; Don't ask me yes/no, I need y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Time stamp
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%04y-%02m-%02d %02M:%02S %:a by %u")

;; Show parentheses in pairs
(show-paren-mode t)

;; Set text-mode for default major mode
(setq default-major-mode 'text-mode)

;; Show line number
(require 'linum)
(global-linum-mode t)
(setq column-number-mode t)
(setq line-number-node t) ;; show line number in status bar
(setq linum-format "%5d ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable linum for certern major mode
(defcustom linum-disabled-modes-list
  '(eshell-mode
    wl-summary-mode
    compilation-mode
    org-mode
    text-mode
    dired-mode
    term-mode
    gud-mode)
  "* List of modes disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'linum
  )
(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *Gnu Emacs*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes
defined in `linum-disabled-modes-list'. Changed by
linum-off. Also turns off numbering in starred modes like
*scratch*"

  (unless (or (minibufferp)
              (member major-mode linum-disabled-modes-list)
              (and
               linum-disable-starred-buffers
               (string-match "*" (buffer-name)))
              )
    (linum-mode 1)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't show the welcome page
(setq inhibit-startup-message t)

;;Replace all freakin' ^M chars in the current buffer
(fset 'replace-ctrlms
      [escape ?< escape ?% ?\C-q ?\C-m return ?\C-q ?\C-j return ?!])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tab Bar
(ray-add-path "plugins/tabbar")
(require 'tabbar)
(tabbar-mode)

;; group all buffers info 4 groups
(defun tabbar-buffer-groups ()
  (list
   (cond
    ((eq major-mode 'cmake-mode)
     "CMake"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((and
      (> (length (buffer-name)) 5)
      (string-equal "*term" (substring (buffer-name) 0 5)))
     "Terminal"
     )
    ((or
      (string-equal ".emacs" (buffer-name))
      (string-equal "_emacs" (buffer-name)))
     "LISP"
     )
    ((and
      (> (length (buffer-name)) 3)
      (string-equal
       ".el"
       (substring (buffer-name) -3 nil)))
     "LISP"
     )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs Buffer"
     )
    (t
     "User Buffer")
    )))
;;;; EOF Tab Bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto restore
;; Session, auto save emacs' statues
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

;; (when (require 'erc-compat nil t)
;;   (defalias 'remove-if-not 'erc-remove-if-not)
;;   (require 'wcy-desktop)
;;   (wcy-desktop-init))
(desktop-save-mode 1)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Open file as root
(defvar find-file-root-prefix (if (featurep 'xemacs)
                                  "/[sudo/root@localhost]"
                                "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto close buffer when thread exit
(add-hook 'shell-mode-hook 'my-close-func)
(add-hook 'gdb-mode-hook 'my-close-func)
(defun my-close-func ()
  (let ((state (get-buffer-process (current-buffer))))
    (if (not (eq state nil))
        (progn
          (set-process-sentinel state
                                'kill-buffer-on-exit))))
  )
(defun kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Go to char, like "f" in vim
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forward to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; encoding
(if is-at-office
    (progn
      (modify-coding-system-alist
       'file "\\.cpp\\'" 'gb18030)
      (modify-coding-system-alist
       'file "\\.h\\'" 'gb18030)
      (modify-coding-system-alist
       'file "\\.txt\\'" 'gb18030))
  )
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;; setup compile command for cmake project
(defun detect-cmake-script ()
  (interactive)
  (setq current-path default-directory
        try-count 0
        max-try-count 10)

  (while (and
          (not (file-exists-p (concat current-path "README")))
          (< try-count max-try-count))
    (message (format "trying path %s" current-path))
    (setq current-path (concat current-path "../"))
    (setq try-count (+ try-count 1))
    )
  (message "t1")
  (if (< try-count max-try-count)
      (progn
        (set 'compile-command
             (format "cd %sbuild; cmake .." current-path))
        (message (format
                  "setting compile command as %s"
                  compile-command))
        )
    )
  )
;; Generic Compilation Window
(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  ;; (detect-cmake-script)
  (progn
    (call-interactively 'compile)
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 15))
    (select-window cur)
    )
  )
(defun my-compilation-hook ()
  "Make sure that the compile window is splitting vertically"
  (progn
    (if (not (get-buffer-window "*compilation*"))
        (progn
          (split-window-vertically)
          )
      )
    )
  )
;(add-hook 'compilation-mode-hook 'my-compilation-hook)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; send esc key in terminal
(defun term-send-escape ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

;; setup modeline
(setq-default
 mode-line-format
 '(
   ;; buffer name
   " %b "
   ;; position
   "(%l,%c) "
   ;; char pos
   "(%p,"
   (:eval (number-to-string (point)))
   "/%i)"
   " "
   (:eval
    (cond (buffer-read-only
           (propertize "RO" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-face))
          (t "    ")))
   (:eval
    (symbol-name
     (car (coding-system-priority-list))))
   " %["
   (:propertize mode-name)
   "%] "
   (global-mode-string global-mode-string)
   "%["
   (:propertize which-func-current)
   "%]"
   ))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
