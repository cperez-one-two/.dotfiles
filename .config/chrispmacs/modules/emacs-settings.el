;;; emacs-settings.el -*- lexical-binding: t; -*-

;; Default emacs settings
; no startup message
(setq inhibit-startup-message t)

;; no scroll bar
(scroll-bar-mode -1)

;; no tool bar at the top
(tool-bar-mode -1)

;; no tooltip popup
(tooltip-mode -1)

;; no menu bar
(menu-bar-mode -1)

;; quick jump to recent files in recentf buffer
(recentf-mode 1)

;; typed M-x command history
(setq history-length 25)
(savehist-mode 1)

;; disable bell on MacOS, keep on Linux
(unless (eq system-type 'darwin)
  (setq visible-bell t))

;; disable manually customized variables from cluttering init file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; line and character numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; auto-revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; auto-revert file buffers on changes from elsewhere
(global-auto-revert-mode 1)

;; move auto-backups to one place
(setq backup-directory-alist `(("." . "~/.saves/")))

;; tab-bar-mode by default
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;; font
(cond
  ((eq system-type 'gnu/linux)
    (defvar cop/default-font-size 140)
    (defvar cop/default-variable-font-size 140))
  ((eq system-type 'darwin)
    (defvar cop/default-font-size 180)
    (defvar cop/default-variable-font-size 180))
  (t
    (message "Config for unknown system type.")))

;; font face attributes
(set-face-attribute 'default nil
                    :font "Iosevka Term SS03"
                    :height cop/default-font-size
                    :weight 'light)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Term SS03"
                    :height cop/default-font-size
                    :weight 'light)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Term SS03"
                    :height cop/default-variable-font-size
                    :weight 'light)
