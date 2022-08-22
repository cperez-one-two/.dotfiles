;; chrisPmacs

;; Defaults
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(unless (eq system-type 'darwin)
  (setq visible-bell t))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; auto-revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; auto-revert file buffers on changes from elsewhere
(global-auto-revert-mode 1)

;; spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; move auto-backups to one place
(setq backup-directory-alist `(("." . "~/.saves/")))

;; tab-bar-mode by default
(tab-bar-mode 1)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;; font
(cond
  ((eq system-type 'gnu/linux)
    (defvar efs/default-font-size 140)
    (defvar efs/default-variable-font-size 140))
  ((eq system-type 'darwin)
    (defvar efs/default-font-size 180)
    (defvar efs/default-variable-font-size 180))
  (t
    (message "Config for unknown system type.")))

;; later used to configure UI elements
(set-face-attribute 'default nil
                    :font "Iosevka Term SS03"
                    :height efs/default-font-size
                    :weight 'light)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Term SS03"
                    :height efs/default-font-size
                    :weight 'light)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Term SS03"
                    :height efs/default-variable-font-size
                    :weight 'light)

;; Initialize package repos
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; org-mode config
(defun cop/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'org)
 (setq org-agenda-files
       '("~/Sync/Resources/denote/20220821T210952--next-actions__meta.org"))
(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-scheduled-past-days 5)
(setq org-todo-keywords
      '((sequence "NEXT(n)" "BACKLOG(b)" "|" "DONE(d!)")))

(setq org-tag-alist
      '((:startgroup)
        (:endgroup)
        ("@home" . ?h)
        ("@computer" . ?c)
        ("@errands" . ?e)
        ("@Calls" . ?C)
        ("@meetings" . ?m)
        ("@waiting" . ?w)
        ("@someday" . ?s)))

(setq org-agenda-custom-commands
      '(("n" "Next Actions"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-prefix-format "  %T %?-12t% s")))
          (tags "+@computer"
                     ((org-agenda-overriding-header "@computer")
                      (org-agenda-max-todos nil)
                      (org-agenda-sorting-strategy '(priority-up))
                      (org-agenda-prefix-format "  %?-12t% s")))
          (tags "+@home"
                     ((org-agenda-overriding-header "@home")
                      (org-agenda-sorting-strategy '(priority-up))
                      (org-agenda-prefix-format "  %?-12t% s")))
          (tags "+@Calls"
                     ((org-agenda-overriding-header "@Calls")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags "+@meetings"
                     ((org-agenda-overriding-header "@meeting-items")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-up))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags "+@errands"
                     ((org-agenda-overriding-header "@errands")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-up))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags "+@waiting"
                     ((org-agenda-overriding-header "@waiting-on")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-up))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))))))

(add-hook 'org-mode-hook 'cop/org-mode-setup)

;; denote - a simlpe note-taking package
(require 'denote)
(setq denote-directory (expand-file-name "~/Sync/Resources/denote"))
(setq denote-known-keywords '("emacs" "testing" "project"))
(setq denote-file-type nil)
;; (setq denote-link-backlinks-display-buffer-action
;;       '((display-buffer-reuse-window
;;          display-buffer-in-side-window)
;;         (side . left)
;;         (slot . 99)
;;         (window-width . 0.3)))

(add-hook 'dired-mode-hook #'denote-dired-mode)

;; evil-mode - because I'm evil... I commit nefarious acts... on my troublemaker shit
(unless (package-installed-p 'evil)
  (package-install 'evil))

;(require 'evil)
;(evil-set-undo-system 'undo-redo)
;(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(package-selected-packages '(denote)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

