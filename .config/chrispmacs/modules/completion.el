;;;; completion.el -*- lexical-binding: t; -*-

;;; Dependencies
(cop-install-package-if-not-already 'vertico)
(cop-install-package-if-not-already 'consult)
(cop-install-package-if-not-already 'savehist)
(cop-install-package-if-not-already 'marginalia)
(cop-install-package-if-not-already 'orderless)

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

(provide 'completion)
