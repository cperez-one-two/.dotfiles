;;; doom-twilight-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-tomorrow-night-theme nil
  "Options for the `doom-twilight' theme."
  :group 'doom-themes)

(defcustom doom-twilight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-twilight-theme
  :type '(choice integer boolean))

(def-doom-theme doom-twilight
  "A theme based off of ChrisP's Twilight"

  ;; name        gui       256       16
  ((bg         '("#000000" nil       nil          ))
   (bg-alt     '("#161719" nil       nil          ))
   (base0      '("#0d0d0d" "black"   "black"      ))
   (base1      '("#1b1b1b" "#1b1b1b"              ))
   (base2      '("#212122" "#1e1e1e"              ))
   (base3      '("#292b2b" "#292929" "brightblack"))
   (base4      '("#3f4040" "#3f3f3f" "brightblack"))
   (base5      '("#5c5e5e" "#525252" "brightblack"))
   (base6      '("#757878" "#6b6b6b" "brightblack"))
   (base7      '("#969896" "#979797" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"      ))
   (fg         '("#c5c8c6" "#c5c5c5" "white"))
   (fg-alt     (doom-darken fg 0.4))

   (grey       '("#5f5a60" "#5f5a60" "brightblack"))
   (red        '("#cf6a4c" "#cf6a4c" "red"))
   (orange     '("#de935f" "#dd9955" "brightred"))
   (yellow     '("#f9ee98" "#f9ee98" "yellow"))
   (green      '("#8f9d6a" "#8f9d6a" "green"))
   (blue       '("#81a2be" "#88aabb" "brightblue"))
   (dark-blue  '("#7587a6" "#7587a6" "blue"))
   (teal       blue) ; FIXME replace with real teal
   (magenta    '("#9b859d" "#9b859d" "magenta"))
   (violet     '("#b294bb" "#b294bb" "brightmagenta"))
   (cyan       '("#afc4db" "#afc4db" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))

   ;; face categories
   (highlight      blue)
   (vertical-bar   base0)
   (selection      `(,(car (doom-lighten bg 0.1)) ,@(cdr base4)))
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.14))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-twilight-padded-modeline
      (if (integerp doom-twilight-padded-modeline)
          doom-twilight-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground blue :bold bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path))

  ;; --- variables --------------------------
  ;; ()
  )

;;; doom-twilight-theme.el ends here
