;;; org-settings.el -*- lexical-binding: t; -*-

;; All my org settings. It can get quite long, so I gave it
;; it's own module.
(setq org-agenda-files
      '("~/Sync/Resources/denote/20220821T210952--next-actions__meta.org" "~/Sync/Resources/denote/20220822T110458--reminders__meta.org"))
(setq org-archive-location "~/Sync/Resources/denote/20220823T213739--completed-tasks__meta.org::")
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

;; org-mode config
(defun cop/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'cop/org-mode-setup)

(provide 'org-settings)
