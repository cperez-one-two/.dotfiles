;;;; note-taking.el -*- lexical-binding: t; -*-

;;; Configuration all revolving around taking notes

;;; Dependencies
(cop-install-package-if-not-already 'denote)

;;; denote - a simlpe note-taking package
(require 'denote)
; config
(defun cop/denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %B %e %Y") ; format like Tuesday June 14 2022
   '("journal")))
(setq denote-directory (expand-file-name "~/Sync/Resources/denote"))
(setq denote-known-keywords '("emacs" "testing" "project"))
(setq denote-file-type nil)
(setq denote-link-fontify-backlinks t)

(let ((map global-map))
  (define-key map (kbd "C-c n j") #'cop/denote-journal) ; our custom command
  (define-key map (kbd "C-c n n") #'denote)
  ;(define-key map (kbd "C-c n N") #'denote-type)
  ;(define-key map (kbd "C-c n d") #'denote-date)
  ;(define-key map (kbd "C-c n s") #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; (setq denote-link-backlinks-display-buffer-action
;;       '((display-buffer-reuse-window
;;          display-buffer-in-side-window)
;;         (side . left)
;;         (slot . 99)
;;         (window-width . 0.3)))

(add-hook 'dired-mode-hook #'denote-dired-mode)

