;;;; completion.el -*- lexical-binding: t; -*-

;;; Dependencies
(cop-install-package-if-not-already 'vertico)
(cop-install-package-if-not-already 'consult)
(cop-install-package-if-not-already 'savehist)
(cop-install-package-if-not-already 'marginalia)
(cop-install-package-if-not-already 'orderless)
(cop-install-package-if-not-already 'corfu)

;; Vertico: Minibuffer output of options
(require 'vertico)
(vertico-mode)
(setq vertico-cycle t)

;; Savehist: Saves minibuffer history
(require 'savehist)
(savehist-mode 1)

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

(require 'corfu)
(setq completion-cycle-threshold 3)
(global-corfu-mode)
(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "<tab>") 'corfu-next)
  (define-key corfu-map (kbd "<backtab>") 'corfu-previous))

(provide 'completion)
