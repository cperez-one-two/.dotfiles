;;;; completion.el -*- lexical-binding: t; -*-

;;; Dependencies
(cop-install-package-if-not-already 'vertico)
(cop-install-package-if-not-already 'savehist)
(cop-install-package-if-not-already 'marginalia)

(require 'vertico)
(vertico-mode)
(setq vertico-cycle t)

(require 'savehist)
(savehist-mode)

(with-eval-after-load 'vertico
  (marginalia-mode))

(provide 'completion)
