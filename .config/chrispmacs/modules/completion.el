;;;; completion.el -*- lexical-binding: t; -*-

;;; Dependencies
(cop-install-package-if-not-already 'vertico)
(cop-install-package-if-not-already 'consult)
(cop-install-package-if-not-already 'savehist)
(cop-install-package-if-not-already 'marginalia)


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

(provide 'completion)
