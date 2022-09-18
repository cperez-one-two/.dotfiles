;;; init.el -*- lexical-binding: t; -*-
;;;
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

;; Initialize package system
(require 'package-system)
(cop-package-initialize)

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; modules to load
(require 'defaults)
(require 'org-settings)
(require 'look-and-feel)
(require 'note-taking)
(require 'terminals)
(require 'coding)

;; TODO :: LSP maybe? Completion menu, idk vertico?
