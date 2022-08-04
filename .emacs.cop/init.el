;; chrisPmacs

;; Defaults
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(unless (eq system-type 'darwin)
  (setq visible-bell t))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; auto-revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; auto-revert file buffers on changes from elsewhere
(global-auto-revert-mode 1)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; move auto-backups to one place
(setq backup-directory-alist `(("." . "~/.saves/")))

;; tab-bar-mode by default
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;; font
(cond
  ((eq system-type 'gnu/linux)
    (defvar efs/default-font-size 140)
    (defvar efs/default-variable-font-size 140))
  ((eq system-type 'darwin)
    (defvar efs/default-font-size 180)
    (defvar efs/default-variable-font-size 180))
  (t
    (message "Config for unknown system type.")))

;; later used to configure UI elements
(set-face-attribute 'default nil :font "Iosevka Term SS03" :height efs/default-font-size :weight 'light)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Term SS03" :height efs/default-font-size :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Iosevka Term SS03" :height efs/default-variable-font-size :weight 'light)

;; Initialize package repos
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; denote - a simlpe note-taking package
(require 'denote)
(setq denote-directory (expand-file-name "~/Sync/Resources/denote"))
(setq denote-known-keywords '("emacs" "denote" "testing"))
(setq denote-file-type nil)
(setq denote-link-backlinks-display-buffer-action
      '((display-buffer-reuse-window
         display-buffer-in-side-window)
        (side . left)
        (slot . 99)
        (window-width . 0.3)))

(add-hook 'dired-mode-hook #'denote-dired-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(package-selected-packages '(denote)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

