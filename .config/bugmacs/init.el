;;; Bugmacs: for bugtesting

(defun bm-package-initialize ()
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(bm-package-initialize)

(defmacro bm-install-package-if-not-already (package)
  "Only install the package if it is not already installed."
  `(unless (package-installed-p ,package) (package-install ,package)))

(load-theme 'modus-vivendi)

(bm-install-package-if-not-already 'consult)
(bm-install-package-if-not-already 'vertico)
(bm-install-package-if-not-already 'savehist)
(bm-install-package-if-not-already 'marginalia)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(marginalia vertico consult)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
