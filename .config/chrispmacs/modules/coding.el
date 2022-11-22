;;; coding.el -*- lexical-binding: t; -*-

;; Configuration for coding goes here. Language-specific settings,
;; syntax highlighting, LSP, etc.

;;; Dependencies
(cop-install-package-if-not-already 'rainbow-delimiters)
(cop-install-package-if-not-already 'tree-sitter)
(cop-install-package-if-not-already 'tree-sitter-langs)
(cop-install-package-if-not-already 'rust-mode)
(cop-install-package-if-not-already 'eglot)

;;; rainbow-delimiters - colors parens for visual aid
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Tree-sitter: smarter, structured syntax highlighting
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)

;;; Rust Lang
(require 'rust-mode)

;;; LSP
(require 'eglot)
(with-eval-after-load 'eglot
  (add-hook 'rust-mode-hook 'eglot-ensure))

(provide 'coding)
