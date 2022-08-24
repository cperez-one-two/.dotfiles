;; chrisPmacs

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "chrisPmacs loaded in %s."
                     (emacs-init-time))))

(defun cop-get-fullpath (@file-relative-path)
  "Return full path of *file-relative-path, relative to caller's file location."
  (concat (file-name-directory (or load-file-name buffer-file-name))
          @file-relative-path))

;; load config modules
(load (cop-get-fullpath "modules/package-system.el"))
(load (cop-get-fullpath "modules/defaults.el"))
(load (cop-get-fullpath "modules/look-and-feel.el"))
(load (cop-get-fullpath "modules/note-taking.el"))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; dired
(require 'dired)
; config
(defun cop/dired-mode-setup ()
  (dired-hide-details-mode 1)
  (cond ((eq system-type 'darwin)
         (setq dired-listing-switches "-ahlF"))
        ((eq system-type 'gnu/linux)
         (setq dired-listing-switches "-ahl --group-directories-first"))))

(add-hook 'dired-mode-hook 'cop/dired-mode-setup)
