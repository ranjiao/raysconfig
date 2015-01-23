(unless (fboundp 'hungry-delete-mode)
   (package-install 'hungry-delete))

(require 'hungry-delete)
(global-hungry-delete-mode)

;; Put all auto save and backup files in one location
(setq temprary-file-directory
      (concat config-base-path "backup"))
(setq backup-directory-alist
      `((".*" . , temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Yassnipet
(require 'yasnippet-bundle)
(setq yas/root-directory
      (concat config-base-path "snippets"))
(yas/load-directory yas/root-directory)

;; Go back to original place from function definition
(defun def-goback()
  (interactive)
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is current empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
         (alist (semantic-mrub-ring-to-assoc-list ring))
         (first (cdr (car alist))))
    (if (semantic-equivalent-tag-p (oref first tag)
                                   (semantic-current-tag))
        (setq first (cdr (car (cdr alist)))))
    (semantic-mrub-switch-tags first)))

