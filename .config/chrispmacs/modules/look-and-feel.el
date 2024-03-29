;;; look-and-feel.el -*- lexical-binding: t; -*-

;; Configuration for UI goes here. Modeline styling, themes, etc.
;;; dependencies
(cop-install-package-if-not-already 'all-the-icons)
(cop-install-package-if-not-already 'doom-themes)
(cop-install-package-if-not-already 'doom-modeline)
(cop-install-package-if-not-already 'autothemer)

;;; doom-themes
;; Load custom theme path
(add-to-list 'custom-theme-load-path "~/.config/chrispmacs/themes")
(require 'doom-themes)
(require 'autothemer)
;; config
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-solarized-dark t)
;(load-theme 'doom-tomorrow-night t)
;(load-theme 'catppuccin-mocha t)

;;; modeline
(require 'doom-modeline)
;; Start up the modeline after initialization is finished
(add-hook 'after-init-hook 'doom-modeline-mode)
;; config
(customize-set-variable 'doom-modeline-height 15)
(customize-set-variable 'doom-modeline-bar-width 6)
(customize-set-variable 'doom-modeline-minor-modes t)
(customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; font settings
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

(provide 'look-and-feel)
