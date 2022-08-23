;;;         __         _      ____
;;;   _____/ /_  _____(_)____/ __ \____ ___  ____ ___________
;;;  / ___/ __ \/ ___/ / ___/ /_/ / __ `__ \/ __ `/ ___/ ___/
;;; / /__/ / / / /  / (__  ) ____/ / / / / / /_/ / /__(__  )
;;; \___/_/ /_/_/  /_/____/_/   /_/ /_/ /_/\__,_/\___/____/
;;;

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
(load (cop-get-fullpath "modules/emacs-settings.el"))
(load (cop-get-fullpath "modules/org-settings.el"))
(load (cop-get-fullpath "modules/look-and-feel.el"))
(load (cop-get-fullpath "modules/note-taking.el"))

;; TODO :: more modules (vcs, misc, eglot?)

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
