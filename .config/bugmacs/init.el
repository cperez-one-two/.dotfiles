;;; Bugmacs: for bugtesting

(defun bm-package-initialize ()
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(bm-package-initialize)

(defmacro bm-install-package-if-not-already (package)
  "Only install the package if it is not already installed."
  `(unless (package-installed-p ,package) (package-install ,package)))

(load-theme 'modus-vivendi t)
(add-to-list 'custom-theme-load-path "~/.config/bugmacs/themes")

;; Default emacs settings
; no startup message
(setq inhibit-startup-message t)

; no scroll bar
(scroll-bar-mode -1)

; no tool bar at the top
(tool-bar-mode -1)

; no tooltip popup
(tooltip-mode -1)

; no menu bar
(menu-bar-mode -1)

; auto revert mode
(global-auto-revert-mode 1)

; tabs
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

; disable manually customized variables from cluttering init file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(bm-install-package-if-not-already 'consult)
(bm-install-package-if-not-already 'vertico)
(bm-install-package-if-not-already 'savehist)
(bm-install-package-if-not-already 'marginalia)
(bm-install-package-if-not-already 'orderless)
;;(bm-install-package-if-not-already 'autothemer)

(require 'vertico)
(vertico-mode)
(setq vertico-cycle t)

(require 'savehist)
(savehist-mode)

(require 'marginalia)
(with-eval-after-load 'vertico
  (marginalia-mode))

(require 'consult)
(setq completion-styles '(substring basic))
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-M-y") 'consult-yank-from-kill-ring)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
