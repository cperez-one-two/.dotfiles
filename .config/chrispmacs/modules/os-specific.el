;;;; os-specific.el -*- lexical-binding: t; -*-

(cop-install-package-if-not-already 'exec-path-from-shell)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  ;(setq exec-path-from-shell-arguments '("))
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "LANG"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(provide 'os-specific)
