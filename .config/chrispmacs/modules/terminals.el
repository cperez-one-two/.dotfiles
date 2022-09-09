;;; terminals.el  -*- lexical-binding: t; -*-

;; Configuration for eshell, shell, term, vterm, etc.

;;; dependencies
(cop-install-package-if-not-already 'vterm)

;;; vterm
(require 'vterm)
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
(setq vterm-max-scrollback 10000)

(provide 'terminals)

