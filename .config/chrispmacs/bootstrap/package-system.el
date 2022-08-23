;;;; package-system.el -*- lexical-binding: t; -*-

;;; do package stuff
(require 'package)

;; Initialize package repos
;; Emacs 27.x has gnu elpa as the default
;; Emacs 28.x adds the nongnu elpa to the list by default, so only
;; need to add nongnu when this isn't Emacs 28+
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                            ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                            ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                            ; from melpa
(customize-set-variable 'package-user-dir
                        (expand-file-name "elpa/" user-emacs-directory))

;; TODO :: make defmacro that can be used in the other modules instead of declaring them herer

(defmacro cop-install-package-if-not-already (package)
  "Only install the package if it is not already installed."
  `(unless (package-installed-p ,package) (package-install ,package)))

;; declare all dependencies here.
;; I hate having to go around package-installing on all these machines
;;(setq package-list
;;'(doom-themes org denote rainbow-delimiters))
(defun cop-package-initialize ()
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

;; Install missing packages
;;(dolist (package package-list)
;;(unless (package-installed-p package)
;;(package-install package)))

(provide 'package-system)
