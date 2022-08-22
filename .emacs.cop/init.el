;; chrisPmacs

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "chrisPmacs loaded in %s."
                     (emacs-init-time))))

;; Defaults
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(recentf-mode 1)
(setq history-length 25)
(savehist-mode 1)
(unless (eq system-type 'darwin)
  (setq visible-bell t))
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

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
    (defvar cop/default-font-size 140)
    (defvar cop/default-variable-font-size 140))
  ((eq system-type 'darwin)
    (defvar cop/default-font-size 180)
    (defvar cop/default-variable-font-size 180))
  (t
    (message "Config for unknown system type.")))

;; later used to configure UI elements
(set-face-attribute 'default nil
                    :font "Iosevka Term SS03"
                    :height cop/default-font-size
                    :weight 'light)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka Term SS03"
                    :height cop/default-font-size
                    :weight 'light)
(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Term SS03"
                    :height cop/default-variable-font-size
                    :weight 'light)

;; Initialize package repos
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-list
      '(doom-themes org denote rainbow-delimiters))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; dired
(require 'dired)
; config
(defun cop/dired-mode-setup ()
  (dired-hide-details-mode 1)
  (cond ((eq system-type 'darwin)
         (setq dired-listing-switches "-ahlF"))
        ((eq system-type 'gnu/linux)
         (setq dired-listing-switches "-ahl --group-directories-first"))))

(add-hook 'dired-mode-hook 'cop/dired-mode-setup)

;; doom-themes
(require 'doom-themes)
;; config
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-tomorrow-night t)


;; org-mode config
(defun cop/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'org)
 (setq org-agenda-files
       '("~/Sync/Resources/denote/20220821T210952--next-actions__meta.org" "~/Sync/Resources/denote/20220822T110458--birthdays__meta.org"))
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

(let ((map global-map))
  (define-key map (kbd "C-c a") #'org-agenda))

(add-hook 'org-mode-hook 'cop/org-mode-setup)

;; denote - a simlpe note-taking package
(require 'denote)
; config
(defun cop/denote-journal ()
  "Create an entry tagged 'journal' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %B %e %Y") ; format like Tuesday June 14 2022
   '("journal")))
(setq denote-directory (expand-file-name "~/Sync/Resources/denote"))
(setq denote-known-keywords '("emacs" "testing" "project"))
(setq denote-file-type nil)
(setq denote-link-fontify-backlinks t)

(let ((map global-map))
  (define-key map (kbd "C-c n j") #'cop/denote-journal) ; our custom command
  (define-key map (kbd "C-c n n") #'denote)
  ;(define-key map (kbd "C-c n N") #'denote-type)
  ;(define-key map (kbd "C-c n d") #'denote-date)
  ;(define-key map (kbd "C-c n s") #'denote-subdirectory)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-link-add-links)
  (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
  (define-key map (kbd "C-c n b") #'denote-link-backlinks)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

;; (setq denote-link-backlinks-display-buffer-action
;;       '((display-buffer-reuse-window
;;          display-buffer-in-side-window)
;;         (side . left)
;;         (slot . 99)
;;         (window-width . 0.3)))

(add-hook 'dired-mode-hook #'denote-dired-mode)

;; rainbow-delimiters - colors parens for visual aid
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
