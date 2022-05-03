;; chrisPmacs
;; remove default fluff
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

;; font
(cond
  ((eq system-type 'gnu/linux)
    (defvar efs/default-font-size 120)
    (defvar efs/default-variable-font-size 120))
  ((eq system-type 'darwin)
    (defvar efs/default-font-size 180)
    (defvar efs/default-variable-font-size 180))
  (t
    (message "Config for unknown system type.")))

;; later used to configure UI elements
(set-face-attribute 'default nil :font "DejaVuSansMono Nerd Font Mono" :height efs/default-font-size :weight 'book)
(set-face-attribute 'fixed-pitch nil :font "DejaVuSansMono Nerd Font Mono" :height efs/default-font-size :weight 'book)
(set-face-attribute 'variable-pitch nil :font "DejaVuSansMono Nerd Font" :height efs/default-variable-font-size :weight 'book)
;; (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height efs/default-font-size)
;; (set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font Mono" :height efs/default-font-size)
;; (set-face-attribute 'variable-pitch nil :font "Iosevka" :height efs/default-variable-font-size :weight 'medium)

;; Initialize package repos
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :demand
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  ;; :custom
  ;; (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; NOTE: The fist time you load this on a new machine, be sure to run:
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes)
  ;;:init (load-theme 'doom-snazzy t)
  ;;:init (load-theme 'doom-gruvbox t)
  ;;:init (load-theme 'doom-horizon t)
  ;;:init (load-theme 'doom-palenight t)
  ;;:init (load-theme 'doom-tomorrow-night t))

(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        ;;modus-themes-region '(bg-only no-extend)
        modus-themes-org-blocks 'gray-background)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.2))

(use-package rainbow-mode)

(use-package general
  :config
  (general-create-definer efs/leader-key
    :prefix "C-c")

  (efs/leader-key
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("n" text-scale-increase "up")
  ("p" text-scale-decrease "down")
  ("q" nil "finish and exit" :exit t))

(efs/leader-key
  "ts"  '(hydra-text-scale/body :which-key "scale text"))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
         ))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; forge: allows you to use a lot of github features from within emacs
;; (use-package forge)

;; (defun efs/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook (lsp-mode . efs/lsp-mode-setup)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (lsp-enable-which-key-integration t))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-position 'bottom))

;; (use-package lsp-ivy)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package eglot
  :hook
  (js2-mode . eglot-ensure))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook (typescript-mode . lsp-deferred)
;;   :config
;;   (setq typescript-indent-level 2))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'"

;; (use-package js2-mode
;;   :mode "\\.js\\'"
;;   :hook (js2-mode . lsp-deferred))
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(unless (eq system-type 'gnu/linux)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ((cond ((eq system-type 'darwin)
          (dired-listing-switches "-ahlF"))
         ((eq system-type 'gnu/linux)
          (dired-listing-switches "-ahl --group-directories-first")))))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/Sync/roam/20220228153956-birthdays.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "BACKLOG(b)" "|" "DONE(d!)")))

  (setq org-todo-keyword-faces
        '(("NEXT" . (:foreground "orange red" :weight bold))
          ("WAIT" . (:foreground "HotPink2" :weight bold))
          ("BACK" . (:foreground "MediumPurple3" :weight bold))))

  (setq org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("@home" . ?H)
          ("@work" . ?W)
          ("batch" . ?b)))
  (setq org-columns-default-format "%20CATEGORY(Category) %65ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock) %TAGS")

  (setq org-agenda-custom-commands
        `(("d" "Work Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (tags-todo "+PRIORITY=\"A\"+@work"
                       ((org-agenda-overriding-header "High Priority")))
            (tags-todo "+TODO=\"NEXT\"+@work"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-max-todos nil)))
            (tags-todo "+TODO=\"TODO\"+@work-batch"
                       ((org-agenda-overriding-header "Active")
                        (org-agenda-files org-agenda-files))
                       (org-agenda-text-search-extra-files nil))
            (tags-todo "+TODO=\"WAITING\"+@work"
                       ((org-agenda-overriding-header "Waiting On External")
                        (org-agenda-files org-agenda-files))
                       (org-agenda-text-search-extra-files nil))
            (tags-todo "+batch+@work" ((org-agenda-overriding-header "Batchable Small Tasks"))))
           ((org-agenda-tag-filter-preset '("+@work"))))
          ("n" "Next Tasks"
           ((agenda "" ((org-deadline-warning-days 7)))
            (tags-todo "+TODO=\"NEXT\"+@work"
                       ((org-agenda-overriding-header "Next Tasks")))))

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0+@work"
           ((org-agenda-overriding-header "Low Effort Work Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("h" "Home Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)
                        ))
            (tags-todo "+PRIORITY=\"A\"+@home"
                       ((org-agenda-overriding-header "High Priority")))

            (tags-todo "+TODO=\"NEXT\"+@home"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-max-todos nil)))
            (tags-todo "+TODO=\"TODO\"+@home-batch"
                       ((org-agenda-overriding-header "Active")
                        (org-agenda-files org-agenda-files))
                       (org-agenda-text-search-extra-files nil))
            (tags-todo "+TODO=\"WAITING\"+@home"
                       ((org-agenda-overriding-header "Waiting On External")
                        (org-agenda-files org-agenda-files))
                       (org-agenda-text-search-extra-files nil))
            (tags-todo "+batch+@home" ((org-agenda-overriding-header "Batchable Small Tasks"))))
           ((org-agenda-tag-filter-preset '("+@home"))))
          ("n" "Next Tasks"
           ((agenda "" ((org-deadline-warning-days 7)))
            (tags-todo "+TODO=\"NEXT\"+@home"
                       ((org-agenda-overriding-header "Next Tasks")))))

          ;; Low-effort next actions
          ("f" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0+@home"
           ((org-agenda-overriding-header "Low Effort Home Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files))))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org-faces
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVuSansMono Nerd Font" :weight 'book :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;;(set-face-attribute 'org-default nil :font "Ubuntu Nerd Font" :weight 'regular)
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package geiser-guile)

(with-eval-after-load 'org
  (require 'scheme)
  (require 'python)
  (setq geiser-active-implementations '(guile))
  (custom-set-variables
   '(scheme-program-name "guile"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (scheme . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/.emacs.efs/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Sync/roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("t" "ticket" plain
      (file "~/Sync/roam/templates/TicketTemplate.org")
      :if-new
      (file+head "tickets/${slug}.org" "#+title: ${title}\n#+category: %^{ticket-id} %^{category}\n#+filetags: Ticket")
      :unnarrowed t)
     ("p" "project" plain
      (file "~/Sync/roam/templates/ProjectTemplate.org")
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("r" "translate request" plain
      (file "~/Sync/roam/templates/TranslateRequestTemplate.org")
      :if-new
      (file+head "translate-requests/%^{ticketid}.org" "#+title: ${title}\n#+filetags: Translate-Request")
      :unnarrowed t)
     ("h" "href" plain
      (file "~/Sync/roam/templates/HrefTemplate.org")
      :if-new
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: %^{category}")
      :unnarrowed t)))
  (setq org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
      :if-new
      (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("What Happened Today"))
      :unnarrowed t
      :empty-lines 1)
     ("j" "journal" entry "* %<%I:%M %p>: %?"
      :if-new
      (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Log"))
      :unnarrowed t
      :empty-lines 1)))
  :bind (("C-c n r" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-goto-yesterday)
         ("y" . org-roam-dailies-capture-yesterday)         
         ("T" . org-roam-dailies-goto-today)
         ("t" . org-roam-dailies-capture-today)         
         ("M" . org-roam-dailies-goto-tomorrow)
         ("m" . org-roam-dailies-capture-tomorrow)         
         ("D" . org-roam-dailies-goto-date)
         ("d" . org-roam-dailies-capture-date)         
         ("f" . org-roam-dailies-goto-next-note)
         ("b" . org-roam-dailies-goto-previous-note))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Completed Tasks"))
              :empty-lines 1)))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today)))))

(use-package vulpea
  :after org-roam
  :config
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update))

(use-package org-jira
:init
(setq org-jira-working-dir "~/.emacs.efs/.org-jira")
:config
(setq jiralib-url "https://wizehive.atlassian.net/"))

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-tree-slide which-key vulpea vterm visual-fill-column use-package typescript-mode rainbow-mode rainbow-delimiters org-jira org-bullets multiple-cursors modus-themes magit lsp-ui lsp-ivy js2-mode ivy-rich ivy-prescient hydra helpful general geiser-mit geiser-guile evil-nerd-commenter eshell-git-prompt eglot doom-themes doom-modeline dockerfile-mode counsel-projectile company-prescient company-box))
 '(scheme-program-name "guile"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
