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
      '((sequence "NEXT(n)" "WAITING(w)" "BACKLOG(b)" "|" "DONE(d!)")))

(setq org-tag-alist
      '((:startgroup)
        (:endgroup)
        ("@email" . ?e)
        ("@sre" . ?s)
        ("@workflow" . ?w)
        ("@dev" . ?d)
        ("@messaging" . ?m)
        ("@meetings" . ?M)
        ("@home" . ?h)
        ("@someday" . ?S)))

(setq org-agenda-custom-commands
      '(("n" "Next Actions"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-prefix-format "  %T %?-12t% s")))
          (tags-todo "+TODO=\"NEXT\"+@sre"
                     ((org-agenda-overriding-header "@SRE")
                      (org-agenda-max-todos nil)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s")))
          (tags-todo "+TODO=\"NEXT\"+@messaging"
                     ((org-agenda-overriding-header "@Messaging")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s")))
          (tags-todo "+TODO=\"NEXT\"+@meetings"
                     ((org-agenda-overriding-header "@Meeting")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"NEXT\"+@email"
                     ((org-agenda-overriding-header "@Email")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"NEXT\"+@dev"
                     ((org-agenda-overriding-header "@Dev")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"NEXT\"+@workflow"
                     ((org-agenda-overriding-header "@Workflow")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"WAITING\""
                     ((org-agenda-overriding-header "@Followup")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))))
        ("h" "Home Actions"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-prefix-format "  %T %?-12t% s")))
          (tags "+@home"
                ((org-agenda-overriding-header "@Home")
                 (org-agenda-max-todos nil)
                 (org-agenda-sorting-strategy '(priority-down))
                 (org-agenda-prefix-format "  %?-12t% s")))))
        ("b" "Backlog Items"
         ((agenda "" ((org-deadline-warning-days 7)
                      (org-agenda-prefix-format "  %T %?-12t% s")))
          (tags-todo "+TODO=\"BACKLOG\"+@sre"
                     ((org-agenda-overriding-header "@SRE")
                      (org-agenda-max-todos nil)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s")))
          (tags-todo "+TODO=\"BACKLOG\"+@messaging"
                     ((org-agenda-overriding-header "@Messaging")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s")))
          (tags-todo "+TODO=\"BACKLOG\"+@meetings"
                     ((org-agenda-overriding-header "@Meeting")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"BACKLOG\"+@email"
                     ((org-agenda-overriding-header "@Email")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"BACKLOG\"+@dev"
                     ((org-agenda-overriding-header "@Dev")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-prefix-format "  %?-12t% s"))
                     (org-agenda-text-search-extra-files nil))
          (tags-todo "+TODO=\"BACKLOG\"+@workflow"
                     ((org-agenda-overriding-header "@Workflow")
                      (org-agenda-files org-agenda-files)
                      (org-agenda-sorting-strategy '(priority-down))
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
