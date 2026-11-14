;;; twilight-horizon-theme.el --- Pastel-vibrant Molokai-inspired theme from a twilight horizon palette -*- lexical-binding: t; -*-

;; Author: Zichun Koh
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes

;;; Commentary:
;;
;; A soothing dark Emacs theme inspired by a flat twilight-horizon landscape
;; and structured after Molokai's classic syntax rhythm.
;;
;; Deep midnight-indigo silhouettes anchor a low-strain dark canvas, while
;; sunset coral-rose, horizon apricot, sunlit buttercream gold, meadow
;; mint-sage, coastal lagoon teal, luminous lake cyan, and dusk lavender
;; give syntax clear, vibrant separation on the pastel side.

;;; Code:

(deftheme twilight-horizon
  "A soothing, pastel-vibrant dark theme inspired by Molokai and a twilight horizon.")

(defgroup twilight-horizon-theme nil
  "Customization options for the `twilight-horizon' theme."
  :group 'faces)

(defcustom twilight-horizon-italic-comments t
  "When non-nil, comments and docstrings are rendered in italics."
  :group 'twilight-horizon-theme
  :type 'boolean)

(defcustom twilight-horizon-italic-functions nil
  "When non-nil, function names are rendered in italics, like classic Molokai."
  :group 'twilight-horizon-theme
  :type 'boolean)

(defcustom twilight-horizon-italic-keywords nil
  "When non-nil, keywords are rendered in italics."
  :group 'twilight-horizon-theme
  :type 'boolean)

(defcustom twilight-horizon-padded-modeline nil
  "When non-nil, add vertical padding around the mode line.
Can be t (uses 4px padding) or an integer pixel width."
  :group 'twilight-horizon-theme
  :type '(choice boolean integer))

(let* ((class '((class color) (min-colors 89)))

       (comment-slant (if twilight-horizon-italic-comments 'italic 'normal))
       (function-slant (if twilight-horizon-italic-functions 'italic 'normal))
       (keyword-slant (if twilight-horizon-italic-keywords 'italic 'normal))
       (modeline-pad (when twilight-horizon-padded-modeline
                       (if (integerp twilight-horizon-padded-modeline)
                           twilight-horizon-padded-modeline
                         4)))

       ;; ------------------------------------------------------------------
       ;; Surfaces & Neutrals (deep mountain indigo -> sun-core ivory)
       ;; ------------------------------------------------------------------
       (bg           "#131521") ; Deep twilight indigo-slate canvas
       (bg-alt       "#181b2a") ; Elevated surface (tabs, inactive buffers, fringe)
       (bg-deep      "#0d0f19") ; Recessed shadow (code blocks, tooltips, inactive bar)
       (bg-subtle    "#1d2133") ; Active mode-line & subtle chrome
       (bg-hl        "#20253a") ; Current line highlight (hl-line)
       (bg-popup     "#1b1f31") ; Popups (corfu, which-key, transient)
       (bg-region    "#2f3757") ; Selection (region)
       (bg-match     "#3b456e") ; Active popup candidate / symbol match
       (border       "#2c334e") ; Dividers & popup borders
       (muted        "#5c658a") ; Line numbers & quiet chrome
       (comment      "#7b85ad") ; Misty horizon slate-periwinkle
       (comment-dim  "#687197") ; Comment delimiters
       (fg-dim       "#9fa8cd") ; Punctuation, annotations, metadata
       (fg-alt       "#cfd5ea") ; Secondary foreground
       (fg           "#f2ede0") ; Sun-cloud warm ivory (main text)
       (fg-bright    "#faf6ec") ; Sun-core cream (bright highlights)

       ;; ------------------------------------------------------------------
       ;; Chromatic Accents (pastel-softened yet vibrant, Molokai rhythm)
       ;; ------------------------------------------------------------------
       (coral        "#f67693") ; Upper sunset sky rose (keywords, Molokai magenta)
       (coral-bright "#fa8ea7") ; Brighter coral highlight
       (salmon       "#f56c73") ; Horizon coral-red (errors, deletions, negation)
       (peach        "#faaa7f") ; Sunset apricot (variables/params, preprocessor)
       (gold         "#f7dd88") ; Giant sun buttercream (strings, warnings)
       (gold-bright  "#fbe7a2") ; Brighter sun buttercream
       (olive-gold   "#d2c58d") ; Sunlit sage-gold (doc-comments)
       (mint         "#91df89") ; Meadow sage-mint (functions, success)
       (mint-soft    "#9de396") ; Softer mint (function calls)
       (teal         "#52dbc7") ; Coastal lagoon teal (builtins, info accents)
       (cyan         "#68daf4") ; Luminous lake cyan (types, links, Molokai cyan)
       (cyan-bright  "#8de5f7") ; Brighter lake cyan
       (cerulean     "#72abf5") ; Deep lake azure (directories, info, branches)
       (cerulean-bright "#94c3fa") ; Brighter lake azure
       (cerulean-dim "#83b8f7") ; Struct fields / object properties
       (lavender     "#c19ef8") ; Dusk horizon violet (constants, numbers)
       (orchid       "#e488c6") ; Twilight berry-orchid (operators, accents)

       ;; ------------------------------------------------------------------
       ;; Blended Tint Surfaces (14% - 24% accent blended onto bg)
       ;; ------------------------------------------------------------------
       (bg-coral     "#332331")
       (bg-coral-hi  "#492c3c")
       (bg-salmon    "#33212c")
       (bg-salmon-hi "#492a35")
       (bg-peach     "#332a2e")
       (bg-gold      "#33312f")
       (bg-gold-hi   "#4a453a")
       (bg-mint      "#253130")
       (bg-mint-hi   "#31453a")
       (bg-teal      "#1c3138")
       (bg-cyan      "#1f313f")
       (bg-cyan-hi   "#274454")
       (bg-cerulean  "#202a3f")
       (bg-lavender  "#2b283f")
       (bg-lavender-hi "#3d3655"))

  (custom-theme-set-faces
   'twilight-horizon

   ;; ------------------------------------------------------------------------
   ;; Core Emacs Faces
   ;; ------------------------------------------------------------------------
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,coral))))
   `(fringe ((,class (:background ,bg :foreground ,muted))))
   `(region ((,class (:background ,bg-region :distant-foreground ,fg-bright :extend t))))
   `(highlight ((,class (:background ,bg-match :foreground ,fg-bright :distant-foreground ,fg-bright))))
   `(hl-line ((,class (:background ,bg-hl :extend t))))
   `(secondary-selection ((,class (:background ,bg-lavender :extend t))))
   `(shadow ((,class (:foreground ,comment))))
   `(minibuffer-prompt ((,class (:foreground ,cyan :weight bold))))
   `(tooltip ((,class (:background ,bg-deep :foreground ,fg))))
   `(vertical-border ((,class (:background ,border :foreground ,border))))
   `(window-divider ((,class (:background ,border :foreground ,border))))
   `(window-divider-first-pixel ((,class (:inherit window-divider))))
   `(window-divider-last-pixel ((,class (:inherit window-divider))))
   `(fill-column-indicator ((,class (:foreground ,border))))
   `(match ((,class (:background ,bg-cyan-hi :foreground ,cyan :weight bold))))
   `(trailing-whitespace ((,class (:background ,bg-salmon-hi :underline (:style wave :color ,salmon)))))
   `(escape-glyph ((,class (:foreground ,lavender))))
   `(homoglyph ((,class (:inherit escape-glyph))))
   `(nobreak-space ((,class (:inherit escape-glyph :underline t))))
   `(nobreak-hyphen ((,class (:inherit nobreak-space))))
   `(link ((,class (:foreground ,cyan :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,orchid :underline t :weight bold))))
   `(error ((,class (:foreground ,salmon :weight bold))))
   `(warning ((,class (:foreground ,gold :weight bold))))
   `(success ((,class (:foreground ,mint :weight bold))))
   `(bold ((,class (:weight bold))))
   `(italic ((,class (:slant italic))))
   `(bold-italic ((,class (:weight bold :slant italic))))
   `(underline ((,class (:underline t))))
   `(button ((,class (:inherit link))))

   ;; Mode-line & Header-line
   `(mode-line
     ((,class (:background ,bg-subtle :foreground ,fg
               :box ,(if modeline-pad
                         `(:line-width ,modeline-pad :color ,bg-subtle)
                       `(:line-width 1 :color ,border))))))
   `(mode-line-active ((,class (:inherit mode-line))))
   `(mode-line-inactive
     ((,class (:background ,bg-deep :foreground ,comment
               :box ,(if modeline-pad
                         `(:line-width ,modeline-pad :color ,bg-deep)
                       `(:line-width 1 :color ,bg-alt))))))
   `(mode-line-emphasis ((,class (:foreground ,coral :weight bold))))
   `(mode-line-highlight ((,class (:background ,bg-match :foreground ,fg-bright))))
   `(mode-line-buffer-id ((,class (:foreground ,fg-bright :weight bold))))
   `(header-line ((,class (:background ,bg-alt :foreground ,fg-alt :box (:line-width 1 :color ,border)))))
   `(header-line-highlight ((,class (:inherit mode-line-highlight))))

   ;; Tab-bar & Tab-line
   `(tab-bar ((,class (:background ,bg-deep :foreground ,comment))))
   `(tab-bar-tab ((,class (:background ,bg :foreground ,fg-bright :weight bold :box (:line-width 2 :color ,bg)))))
   `(tab-bar-tab-inactive ((,class (:background ,bg-alt :foreground ,comment :box (:line-width 2 :color ,bg-alt)))))
   `(tab-line ((,class (:background ,bg-deep :foreground ,comment))))
   `(tab-line-tab ((,class (:background ,bg :foreground ,fg))))
   `(tab-line-tab-current ((,class (:background ,bg :foreground ,fg-bright :weight bold))))
   `(tab-line-tab-inactive ((,class (:background ,bg-alt :foreground ,comment))))
   `(tab-line-tab-inactive-alternate ((,class (:inherit tab-line-tab-inactive))))
   `(tab-line-highlight ((,class (:background ,bg-hl :foreground ,fg-bright))))
   `(tab-line-close-highlight ((,class (:foreground ,coral))))

   ;; Line numbers
   `(line-number ((,class (:inherit default :foreground ,muted :weight normal :slant normal :underline nil :strike-through nil))))
   `(line-number-current-line ((,class (:inherit (hl-line default) :foreground ,gold :weight bold :slant normal :underline nil :strike-through nil))))
   `(linum ((,class (:inherit line-number))))

   ;; Search & Paren matching
   `(isearch ((,class (:background ,gold :foreground ,bg-deep :weight bold))))
   `(isearch-fail ((,class (:background ,bg-salmon-hi :foreground ,salmon :weight bold :strike-through t))))
   `(lazy-highlight ((,class (:background ,bg-cyan-hi :foreground ,fg-bright :weight bold))))
   `(show-paren-match ((,class (:background ,bg-match :foreground ,peach :weight ultra-bold))))
   `(show-paren-mismatch ((,class (:background ,bg-salmon-hi :foreground ,salmon :weight ultra-bold))))
   `(show-paren-match-expression ((,class (:background ,bg-hl))))
   `(pulse-highlight-start-face ((,class (:background ,bg-lavender-hi :extend t))))

   ;; ------------------------------------------------------------------------
   ;; Font-lock (Classic + Emacs 29+ Tree-sitter)
   ;; ------------------------------------------------------------------------
   `(font-lock-builtin-face ((,class (:foreground ,teal))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant ,comment-slant))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment-dim :slant ,comment-slant))))
   `(font-lock-doc-face ((,class (:foreground ,olive-gold :slant ,comment-slant))))
   `(font-lock-doc-markup-face ((,class (:foreground ,peach :slant ,comment-slant))))
   `(font-lock-constant-face ((,class (:foreground ,lavender))))
   `(font-lock-function-name-face ((,class (:foreground ,mint :slant ,function-slant))))
   `(font-lock-function-call-face ((,class (:foreground ,mint-soft :slant ,function-slant))))
   `(font-lock-keyword-face ((,class (:foreground ,coral :slant ,keyword-slant))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face ((,class (:foreground ,peach))))
   `(font-lock-variable-use-face ((,class (:foreground ,fg))))
   `(font-lock-property-name-face ((,class (:foreground ,cerulean-dim))))
   `(font-lock-property-use-face ((,class (:foreground ,cerulean-dim))))
   `(font-lock-string-face ((,class (:foreground ,gold))))
   `(font-lock-number-face ((,class (:foreground ,lavender))))
   `(font-lock-operator-face ((,class (:foreground ,orchid))))
   `(font-lock-negation-char-face ((,class (:foreground ,salmon :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,peach :weight semi-bold))))
   `(font-lock-preprocessor-char-face ((,class (:foreground ,peach :weight semi-bold))))
   `(font-lock-escape-face ((,class (:foreground ,lavender :weight bold))))
   `(font-lock-regexp-face ((,class (:foreground ,gold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,teal :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,coral :weight bold))))
   `(font-lock-bracket-face ((,class (:foreground ,fg-alt))))
   `(font-lock-delimiter-face ((,class (:foreground ,fg-dim))))
   `(font-lock-punctuation-face ((,class (:foreground ,fg-dim))))
   `(font-lock-misc-punctuation-face ((,class (:foreground ,fg-dim))))
   `(font-lock-warning-face ((,class (:foreground ,gold :weight bold))))

   ;; ------------------------------------------------------------------------
   ;; Built-in & Core Modes
   ;; ------------------------------------------------------------------------

   ;; ansi-color / term
   `(ansi-color-black ((,class (:foreground ,bg-deep :background ,bg-deep))))
   `(ansi-color-red ((,class (:foreground ,salmon :background ,salmon))))
   `(ansi-color-green ((,class (:foreground ,mint :background ,mint))))
   `(ansi-color-yellow ((,class (:foreground ,gold :background ,gold))))
   `(ansi-color-blue ((,class (:foreground ,cerulean :background ,cerulean))))
   `(ansi-color-magenta ((,class (:foreground ,coral :background ,coral))))
   `(ansi-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white ((,class (:foreground ,fg :background ,fg))))
   `(ansi-color-bright-black ((,class (:foreground ,comment :background ,comment))))
   `(ansi-color-bright-red ((,class (:foreground ,coral-bright :background ,coral-bright))))
   `(ansi-color-bright-green ((,class (:foreground ,mint-soft :background ,mint-soft))))
   `(ansi-color-bright-yellow ((,class (:foreground ,gold-bright :background ,gold-bright))))
   `(ansi-color-bright-blue ((,class (:foreground ,cerulean-bright :background ,cerulean-bright))))
   `(ansi-color-bright-magenta ((,class (:foreground ,lavender :background ,lavender))))
   `(ansi-color-bright-cyan ((,class (:foreground ,cyan-bright :background ,cyan-bright))))
   `(ansi-color-bright-white ((,class (:foreground ,fg-bright :background ,fg-bright))))

   `(term-color-black ((,class (:inherit ansi-color-black))))
   `(term-color-red ((,class (:inherit ansi-color-red))))
   `(term-color-green ((,class (:inherit ansi-color-green))))
   `(term-color-yellow ((,class (:inherit ansi-color-yellow))))
   `(term-color-blue ((,class (:inherit ansi-color-blue))))
   `(term-color-magenta ((,class (:inherit ansi-color-magenta))))
   `(term-color-cyan ((,class (:inherit ansi-color-cyan))))
   `(term-color-white ((,class (:inherit ansi-color-white))))

   ;; compilation
   `(compilation-column-number ((,class (:foreground ,comment))))
   `(compilation-line-number ((,class (:foreground ,cerulean))))
   `(compilation-error ((,class (:inherit error))))
   `(compilation-warning ((,class (:inherit warning))))
   `(compilation-info ((,class (:inherit success))))
   `(compilation-mode-line-exit ((,class (:foreground ,mint :weight bold))))
   `(compilation-mode-line-fail ((,class (:foreground ,salmon :weight bold))))

   ;; completions
   `(completions-annotations ((,class (:foreground ,fg-dim))))
   `(completions-common-part ((,class (:foreground ,cyan :weight bold))))
   `(completions-first-difference ((,class (:foreground ,coral :weight bold))))
   `(completions-group-title ((,class (:foreground ,cerulean :slant italic))))
   `(completions-group-separator ((,class (:foreground ,border :strike-through t))))
   `(completions-highlight ((,class (:background ,bg-match :extend t))))

   ;; custom & widgets
   `(custom-button ((,class (:background ,bg-subtle :foreground ,fg :box (:line-width 2 :color ,border)))))
   `(custom-button-pressed ((,class (:background ,bg-region :foreground ,fg-bright :box (:line-width 2 :color ,border)))))
   `(custom-button-unraised ((,class (:inherit custom-button))))
   `(custom-button-pressed-unraised ((,class (:inherit custom-button-pressed))))
   `(custom-button-mouse ((,class (:background ,bg-match :foreground ,fg-bright :box (:line-width 2 :color ,cyan)))))
   `(custom-variable-button ((,class (:inherit link :foreground ,mint))))
   `(custom-variable-tag ((,class (:foreground ,cyan :weight bold))))
   `(custom-face-tag ((,class (:foreground ,coral :weight bold))))
   `(custom-group-tag ((,class (:foreground ,fg-bright :weight bold))))
   `(custom-group-tag-1 ((,class (:foreground ,peach :weight bold))))
   `(custom-group-subtitle ((,class (:foreground ,fg-alt :weight bold))))
   `(custom-state ((,class (:foreground ,mint))))
   `(custom-saved ((,class (:foreground ,mint :weight bold))))
   `(custom-modified ((,class (:foreground ,cerulean :weight bold))))
   `(custom-changed ((,class (:foreground ,cerulean))))
   `(custom-set ((,class (:foreground ,gold))))
   `(custom-themed ((,class (:foreground ,gold))))
   `(custom-invalid ((,class (:foreground ,salmon :weight bold))))
   `(custom-visibility ((,class (:foreground ,cerulean))))
   `(widget-field ((,class (:background ,bg-subtle :foreground ,fg :box (:line-width -1 :color ,border) :extend t))))
   `(widget-single-line-field ((,class (:background ,bg-subtle :foreground ,fg))))
   `(widget-inactive ((,class (:foreground ,comment :background ,bg-alt))))
   `(widget-documentation ((,class (:foreground ,mint))))

   ;; diff-mode
   `(diff-added ((,class (:background ,bg-mint :foreground ,mint :extend t))))
   `(diff-removed ((,class (:background ,bg-salmon :foreground ,salmon :extend t))))
   `(diff-changed ((,class (:background ,bg-gold :foreground ,gold :extend t))))
   `(diff-context ((,class (:foreground ,fg-dim))))
   `(diff-header ((,class (:background ,bg-alt :foreground ,fg-bright :weight bold))))
   `(diff-file-header ((,class (:background ,bg-subtle :foreground ,cyan :weight bold))))
   `(diff-hunk-header ((,class (:background ,bg-lavender :foreground ,lavender :weight bold))))
   `(diff-indicator-added ((,class (:foreground ,mint :weight bold))))
   `(diff-indicator-removed ((,class (:foreground ,salmon :weight bold))))
   `(diff-indicator-changed ((,class (:foreground ,gold :weight bold))))
   `(diff-refine-added ((,class (:background ,bg-mint-hi :foreground ,fg-bright :weight bold))))
   `(diff-refine-removed ((,class (:background ,bg-salmon-hi :foreground ,fg-bright :weight bold))))
   `(diff-refine-changed ((,class (:background ,bg-gold-hi :foreground ,fg-bright :weight bold))))

   ;; dired
   `(dired-directory ((,class (:foreground ,cerulean :weight bold))))
   `(dired-ignored ((,class (:foreground ,comment))))
   `(dired-flagged ((,class (:foreground ,salmon :weight bold))))
   `(dired-header ((,class (:foreground ,coral :weight bold))))
   `(dired-mark ((,class (:foreground ,peach :weight bold))))
   `(dired-marked ((,class (:foreground ,coral :weight bold))))
   `(dired-perm-write ((,class (:foreground ,fg-alt :underline t))))
   `(dired-symlink ((,class (:foreground ,cyan :weight bold))))
   `(dired-warning ((,class (:foreground ,gold))))

   ;; ediff
   `(ediff-current-diff-A ((,class (:background ,bg-salmon))))
   `(ediff-current-diff-B ((,class (:background ,bg-mint))))
   `(ediff-current-diff-C ((,class (:background ,bg-gold))))
   `(ediff-fine-diff-A ((,class (:background ,bg-salmon-hi :weight bold))))
   `(ediff-fine-diff-B ((,class (:background ,bg-mint-hi :weight bold))))
   `(ediff-fine-diff-C ((,class (:background ,bg-gold-hi :weight bold))))
   `(ediff-even-diff-A ((,class (:background ,bg-alt))))
   `(ediff-even-diff-B ((,class (:inherit ediff-even-diff-A))))
   `(ediff-even-diff-C ((,class (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-A ((,class (:background ,bg-hl))))
   `(ediff-odd-diff-B ((,class (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-C ((,class (:inherit ediff-odd-diff-A))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,coral :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,orchid))))
   `(eshell-ls-backup ((,class (:foreground ,comment))))
   `(eshell-ls-clutter ((,class (:foreground ,comment))))
   `(eshell-ls-directory ((,class (:foreground ,cerulean :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,mint :weight bold))))
   `(eshell-ls-missing ((,class (:foreground ,salmon))))
   `(eshell-ls-product ((,class (:foreground ,peach))))
   `(eshell-ls-readonly ((,class (:foreground ,lavender))))
   `(eshell-ls-special ((,class (:foreground ,gold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyan))))
   `(eshell-ls-unreadable ((,class (:foreground ,comment))))

   ;; flymake & flyspell
   `(flymake-error ((,class (:underline (:style wave :color ,salmon)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,gold)))))
   `(flymake-note ((,class (:underline (:style wave :color ,teal)))))
   `(flyspell-incorrect ((,class (:underline (:style wave :color ,salmon)))))
   `(flyspell-duplicate ((,class (:underline (:style wave :color ,gold)))))

   ;; help & woman
   `(help-key-binding ((,class (:background ,bg-subtle :foreground ,cyan :box (:line-width -1 :color ,border)))))
   `(help-for-help-header ((,class (:foreground ,fg-bright :weight bold))))
   `(woman-addition ((,class (:foreground ,lavender))))
   `(woman-bold ((,class (:foreground ,coral :weight bold))))
   `(woman-italic ((,class (:foreground ,mint :slant italic))))
   `(woman-unknown ((,class (:foreground ,cyan))))

   ;; hi-lock
   `(hi-yellow ((,class (:background ,bg-gold-hi :foreground ,gold))))
   `(hi-pink ((,class (:background ,bg-coral-hi :foreground ,coral))))
   `(hi-green ((,class (:background ,bg-mint-hi :foreground ,mint))))
   `(hi-blue ((,class (:background ,bg-cyan-hi :foreground ,cyan))))
   `(hi-salmon ((,class (:background ,bg-peach :foreground ,peach))))
   `(hi-aquamarine ((,class (:background ,bg-teal :foreground ,teal))))

   ;; ido
   `(ido-first-match ((,class (:foreground ,mint :weight bold))))
   `(ido-only-match ((,class (:foreground ,teal :weight bold))))
   `(ido-subdir ((,class (:foreground ,cerulean))))
   `(ido-indicator ((,class (:background ,bg-salmon :foreground ,salmon))))
   `(ido-virtual ((,class (:foreground ,comment))))

   ;; makefile & sh
   `(makefile-targets ((,class (:foreground ,mint))))
   `(sh-heredoc ((,class (:foreground ,gold :slant italic))))
   `(sh-quoted-exec ((,class (:foreground ,coral))))

   ;; message
   `(message-header-name ((,class (:foreground ,mint))))
   `(message-header-subject ((,class (:foreground ,cyan :weight bold))))
   `(message-header-to ((,class (:foreground ,coral :weight bold))))
   `(message-header-cc ((,class (:foreground ,cerulean))))
   `(message-header-other ((,class (:foreground ,lavender))))
   `(message-header-newsgroups ((,class (:foreground ,gold))))
   `(message-header-xheader ((,class (:foreground ,fg-dim))))
   `(message-separator ((,class (:foreground ,comment))))
   `(message-mml ((,class (:foreground ,comment :slant italic))))
   `(message-cited-text-1 ((,class (:foreground ,orchid))))
   `(message-cited-text-2 ((,class (:foreground ,teal))))
   `(message-cited-text-3 ((,class (:foreground ,peach))))
   `(message-cited-text-4 ((,class (:foreground ,cerulean))))

   ;; outline (shared with org-level-N)
   `(outline-1 ((,class (:foreground ,cyan :weight bold))))
   `(outline-2 ((,class (:foreground ,coral :weight bold))))
   `(outline-3 ((,class (:foreground ,mint :weight bold))))
   `(outline-4 ((,class (:foreground ,lavender :weight bold))))
   `(outline-5 ((,class (:foreground ,peach :weight bold))))
   `(outline-6 ((,class (:foreground ,teal :weight bold))))
   `(outline-7 ((,class (:foreground ,cerulean :weight bold))))
   `(outline-8 ((,class (:foreground ,gold :weight bold))))

   ;; org-mode
   `(org-archived ((,class (:foreground ,comment))))
   `(org-block ((,class (:background ,bg-deep :extend t))))
   `(org-block-background ((,class (:background ,bg-deep :extend t))))
   `(org-block-begin-line ((,class (:background ,bg-alt :foreground ,fg-dim :extend t))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(org-checkbox ((,class (:foreground ,cyan :weight bold))))
   `(org-checkbox-statistics-done ((,class (:inherit org-done))))
   `(org-checkbox-statistics-todo ((,class (:inherit org-todo))))
   `(org-cite ((,class (:foreground ,teal))))
   `(org-cite-key ((,class (:foreground ,cyan :underline t))))
   `(org-code ((,class (:background ,bg-alt :foreground ,peach))))
   `(org-date ((,class (:foreground ,cerulean :underline t))))
   `(org-document-info ((,class (:foreground ,lavender))))
   `(org-document-info-keyword ((,class (:foreground ,comment))))
   `(org-document-title ((,class (:foreground ,coral :weight bold))))
   `(org-done ((,class (:foreground ,mint :weight bold :strike-through nil))))
   `(org-drawer ((,class (:foreground ,comment))))
   `(org-ellipsis ((,class (:foreground ,peach :underline nil))))
   `(org-footnote ((,class (:foreground ,lavender :underline t))))
   `(org-formula ((,class (:foreground ,teal))))
   `(org-headline-done ((,class (:foreground ,comment))))
   `(org-hide ((,class (:foreground ,bg))))
   `(org-latex-and-related ((,class (:foreground ,peach))))
   `(org-level-1 ((,class (:inherit outline-1))))
   `(org-level-2 ((,class (:inherit outline-2))))
   `(org-level-3 ((,class (:inherit outline-3))))
   `(org-level-4 ((,class (:inherit outline-4))))
   `(org-level-5 ((,class (:inherit outline-5))))
   `(org-level-6 ((,class (:inherit outline-6))))
   `(org-level-7 ((,class (:inherit outline-7))))
   `(org-level-8 ((,class (:inherit outline-8))))
   `(org-link ((,class (:inherit link))))
   `(org-list-dt ((,class (:foreground ,cyan :weight bold))))
   `(org-meta-line ((,class (:foreground ,comment))))
   `(org-priority ((,class (:foreground ,peach :weight bold))))
   `(org-property-value ((,class (:foreground ,fg-alt))))
   `(org-quote ((,class (:background ,bg-alt :foreground ,fg-alt :slant italic :extend t))))
   `(org-special-keyword ((,class (:foreground ,lavender))))
   `(org-table ((,class (:foreground ,cerulean))))
   `(org-tag ((,class (:foreground ,fg-dim :weight normal))))
   `(org-todo ((,class (:foreground ,coral :weight bold))))
   `(org-verbatim ((,class (:background ,bg-alt :foreground ,teal))))
   `(org-verse ((,class (:inherit org-quote))))
   `(org-warning ((,class (:inherit warning))))

   ;; org-agenda
   `(org-agenda-structure ((,class (:foreground ,fg-bright :weight bold))))
   `(org-agenda-date ((,class (:foreground ,cerulean :weight bold))))
   `(org-agenda-date-today ((,class (:foreground ,coral :weight bold :underline t))))
   `(org-agenda-date-weekend ((,class (:foreground ,lavender :weight bold))))
   `(org-agenda-done ((,class (:inherit org-done))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-agenda-clocking ((,class (:background ,bg-cyan))))
   `(org-upcoming-deadline ((,class (:foreground ,salmon))))
   `(org-upcoming-distant-deadline ((,class (:foreground ,peach))))
   `(org-scheduled ((,class (:foreground ,fg))))
   `(org-scheduled-today ((,class (:foreground ,mint))))
   `(org-scheduled-previously ((,class (:foreground ,gold))))
   `(org-time-grid ((,class (:foreground ,muted))))

   ;; re-builder
   `(reb-match-0 ((,class (:background ,bg-cyan-hi :foreground ,cyan :weight bold))))
   `(reb-match-1 ((,class (:background ,bg-coral-hi :foreground ,coral :weight bold))))
   `(reb-match-2 ((,class (:background ,bg-mint-hi :foreground ,mint :weight bold))))
   `(reb-match-3 ((,class (:background ,bg-gold-hi :foreground ,gold :weight bold))))

   ;; whitespace
   `(whitespace-empty ((,class (:background ,bg-alt))))
   `(whitespace-hspace ((,class (:foreground ,border))))
   `(whitespace-space ((,class (:foreground ,border))))
   `(whitespace-tab ((,class (:foreground ,border))))
   `(whitespace-newline ((,class (:foreground ,border))))
   `(whitespace-line ((,class (:background ,bg-salmon :foreground ,salmon))))
   `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
   `(whitespace-indentation ((,class (:background ,bg-alt :foreground ,muted))))
   `(whitespace-space-after-tab ((,class (:background ,bg-gold :foreground ,gold))))
   `(whitespace-space-before-tab ((,class (:inherit whitespace-space-after-tab))))
   `(whitespace-missing-newline-at-eof ((,class (:inherit whitespace-space-after-tab))))

   ;; xref
   `(xref-file-header ((,class (:foreground ,mint :weight bold))))
   `(xref-line-number ((,class (:foreground ,cerulean))))
   `(xref-match ((,class (:inherit match))))

   ;; ------------------------------------------------------------------------
   ;; External Packages
   ;; ------------------------------------------------------------------------

   ;; anzu
   `(anzu-mode-line ((,class (:foreground ,cyan :weight bold))))
   `(anzu-mode-line-no-match ((,class (:foreground ,salmon :weight bold))))
   `(anzu-replace-highlight ((,class (:background ,bg-salmon-hi :foreground ,salmon :weight bold :strike-through t))))
   `(anzu-replace-to ((,class (:background ,bg-mint-hi :foreground ,mint :weight bold))))

   ;; avy
   `(avy-background-face ((,class (:foreground ,muted))))
   `(avy-lead-face ((,class (:background ,coral :foreground ,bg-deep :weight bold))))
   `(avy-lead-face-0 ((,class (:background ,cyan :foreground ,bg-deep :weight bold))))
   `(avy-lead-face-1 ((,class (:background ,gold :foreground ,bg-deep :weight bold))))
   `(avy-lead-face-2 ((,class (:background ,lavender :foreground ,bg-deep :weight bold))))

   ;; bm (visual bookmarks)
   `(bm-face ((,class (:background ,bg-peach :foreground ,peach :extend t))))
   `(bm-persistent-face ((,class (:background ,bg-cyan :foreground ,cyan :extend t))))
   `(bm-fringe-face ((,class (:background ,peach :foreground ,bg-deep))))
   `(bm-fringe-persistent-face ((,class (:background ,cyan :foreground ,bg-deep))))

   ;; company
   `(company-tooltip ((,class (:background ,bg-popup :foreground ,fg))))
   `(company-tooltip-common ((,class (:foreground ,cyan :weight bold))))
   `(company-tooltip-search ((,class (:background ,bg-cyan-hi :foreground ,fg-bright :weight bold))))
   `(company-tooltip-search-selection ((,class (:background ,bg-match :foreground ,cyan :weight bold))))
   `(company-tooltip-selection ((,class (:background ,bg-match :foreground ,fg-bright :weight bold))))
   `(company-tooltip-mouse ((,class (:background ,bg-region))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg ((,class (:background ,bg-alt))))
   `(company-scrollbar-fg ((,class (:background ,muted))))
   `(company-preview ((,class (:foreground ,comment))))
   `(company-preview-common ((,class (:background ,bg-popup :foreground ,cyan))))
   `(company-preview-search ((,class (:inherit company-tooltip-search))))
   `(company-template-field ((,class (:inherit match))))

   ;; consult
   `(consult-preview-line ((,class (:background ,bg-hl :extend t))))
   `(consult-preview-match ((,class (:inherit match))))
   `(consult-preview-insertion ((,class (:foreground ,comment))))
   `(consult-highlight-match ((,class (:foreground ,cyan :weight bold))))
   `(consult-highlight-mark ((,class (:foreground ,coral :weight bold))))
   `(consult-narrow-indicator ((,class (:foreground ,peach :weight bold))))
   `(consult-async-running ((,class (:foreground ,cyan))))
   `(consult-async-finished ((,class (:foreground ,mint))))
   `(consult-async-failed ((,class (:foreground ,salmon))))
   `(consult-async-split ((,class (:foreground ,coral))))
   `(consult-help ((,class (:foreground ,comment))))
   `(consult-key ((,class (:foreground ,teal))))
   `(consult-line-number ((,class (:foreground ,cerulean))))
   `(consult-line-number-prefix ((,class (:foreground ,comment))))
   `(consult-line-number-wrapped ((,class (:foreground ,peach))))
   `(consult-file ((,class (:foreground ,fg-alt))))
   `(consult-grep-context ((,class (:foreground ,comment))))
   `(consult-bookmark ((,class (:foreground ,lavender))))
   `(consult-buffer ((,class (:foreground ,fg))))
   `(consult-imenu-prefix ((,class (:foreground ,orchid))))

   ;; copilot
   `(copilot-overlay-face ((,class (:foreground ,comment :slant italic))))

   ;; corfu
   `(corfu-default ((,class (:background ,bg-popup :foreground ,fg))))
   `(corfu-current ((,class (:background ,bg-match :foreground ,fg-bright :weight semi-bold))))
   `(corfu-bar ((,class (:background ,comment))))
   `(corfu-border ((,class (:background ,border))))
   `(corfu-annotations ((,class (:foreground ,fg-dim :slant italic))))
   `(corfu-deprecated ((,class (:foreground ,comment :strike-through t))))
   `(corfu-echo ((,class (:inherit font-lock-doc-face))))
   `(corfu-popupinfo ((,class (:background ,bg-alt :foreground ,fg))))
   `(corfu-indexed ((,class (:foreground ,peach :weight bold))))
   `(corfu-mouse ((,class (:background ,bg-region))))
   `(corfu-quick1 ((,class (:background ,coral :foreground ,bg-deep :weight bold))))
   `(corfu-quick2 ((,class (:background ,cyan :foreground ,bg-deep :weight bold))))

   ;; deadgrep
   `(deadgrep-meta-face ((,class (:foreground ,comment))))
   `(deadgrep-filename-face ((,class (:foreground ,cyan :weight bold))))
   `(deadgrep-search-term-face ((,class (:foreground ,coral :weight bold))))
   `(deadgrep-regexp-metachar-face ((,class (:foreground ,lavender :weight bold))))
   `(deadgrep-match-face ((,class (:inherit match))))

   ;; diff-hl
   `(diff-hl-insert ((,class (:foreground ,mint :background ,bg-mint))))
   `(diff-hl-delete ((,class (:foreground ,salmon :background ,bg-salmon))))
   `(diff-hl-change ((,class (:foreground ,cerulean :background ,bg-cerulean))))
   `(diff-hl-margin-insert ((,class (:inherit diff-hl-insert))))
   `(diff-hl-margin-delete ((,class (:inherit diff-hl-delete))))
   `(diff-hl-margin-change ((,class (:inherit diff-hl-change))))

   ;; doom-modeline
   `(doom-modeline ((,class (:inherit mode-line))))
   `(doom-modeline-bar ((,class (:background ,coral))))
   `(doom-modeline-bar-inactive ((,class (:background ,bg-alt))))
   `(doom-modeline-buffer-file ((,class (:foreground ,fg-bright :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,cyan :weight bold))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,peach :weight bold))))
   `(doom-modeline-buffer-major-mode ((,class (:foreground ,mint :weight bold))))
   `(doom-modeline-buffer-minor-mode ((,class (:foreground ,fg-dim))))
   `(doom-modeline-project-parent-dir ((,class (:foreground ,fg-dim))))
   `(doom-modeline-project-dir ((,class (:foreground ,mint :weight bold))))
   `(doom-modeline-project-root-dir ((,class (:foreground ,teal :weight bold))))
   `(doom-modeline-panel ((,class (:background ,coral :foreground ,bg-deep :weight bold))))
   `(doom-modeline-host ((,class (:foreground ,lavender :slant italic))))
   `(doom-modeline-debug ((,class (:foreground ,peach))))
   `(doom-modeline-info ((,class (:foreground ,teal :weight bold))))
   `(doom-modeline-warning ((,class (:foreground ,gold :weight bold))))
   `(doom-modeline-urgent ((,class (:foreground ,salmon :weight bold))))
   `(doom-modeline-notification ((,class (:foreground ,coral :weight bold))))
   `(doom-modeline-emphasis ((,class (:foreground ,cyan :weight bold))))
   `(doom-modeline-highlight ((,class (:foreground ,fg-bright :weight bold))))
   `(doom-modeline-lsp-success ((,class (:foreground ,mint :weight bold))))
   `(doom-modeline-lsp-warning ((,class (:foreground ,gold :weight bold))))
   `(doom-modeline-lsp-error ((,class (:foreground ,salmon :weight bold))))
   `(doom-modeline-lsp-running ((,class (:foreground ,cerulean :weight bold))))

   ;; eglot
   `(eglot-highlight-symbol-face ((,class (:background ,bg-hl :weight bold))))
   `(eglot-diagnostic-tag-unnecessary-face ((,class (:foreground ,comment :underline (:style wave :color ,comment)))))
   `(eglot-diagnostic-tag-deprecated-face ((,class (:strike-through t))))
   `(eglot-inlay-hint-face ((,class (:foreground ,comment :height 0.9))))
   `(eglot-parameter-hint-face ((,class (:inherit eglot-inlay-hint-face :slant italic))))
   `(eglot-type-hint-face ((,class (:inherit eglot-inlay-hint-face))))

   ;; eldoc
   `(eldoc-highlight-function-argument ((,class (:foreground ,peach :weight bold))))
   `(eldoc-box-body ((,class (:background ,bg-popup :foreground ,fg))))
   `(eldoc-box-border ((,class (:background ,border))))
   `(eldoc-frame-default ((,class (:background ,bg-popup :foreground ,fg))))
   `(eldoc-frame-border ((,class (:background ,border))))

   ;; embark
   `(embark-keybinding ((,class (:foreground ,cyan :weight bold))))
   `(embark-collect-marked ((,class (:background ,bg-coral :foreground ,coral))))

   ;; flycheck & flyover
   `(flycheck-error ((,class (:underline (:style wave :color ,salmon)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,gold)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,teal)))))
   `(flycheck-fringe-error ((,class (:foreground ,salmon))))
   `(flycheck-fringe-warning ((,class (:foreground ,gold))))
   `(flycheck-fringe-info ((,class (:foreground ,teal))))
   `(flycheck-error-list-highlight ((,class (:inherit hl-line :weight bold))))
   `(flycheck-posframe-face ((,class (:background ,bg-popup :foreground ,fg))))
   `(flycheck-posframe-background-face ((,class (:background ,bg-popup))))
   `(flycheck-posframe-error-face ((,class (:foreground ,salmon))))
   `(flycheck-posframe-warning-face ((,class (:foreground ,gold))))
   `(flycheck-posframe-info-face ((,class (:foreground ,teal))))
   `(flyover-error ((,class (:inherit error))))
   `(flyover-warning ((,class (:inherit warning))))
   `(flyover-info ((,class (:foreground ,teal))))
   `(flyover-marker ((,class (:foreground ,cyan :weight bold))))

   ;; git-gutter / git-commit
   `(git-gutter:added ((,class (:foreground ,mint))))
   `(git-gutter:deleted ((,class (:foreground ,salmon))))
   `(git-gutter:modified ((,class (:foreground ,cerulean))))
   `(git-gutter+-added ((,class (:inherit git-gutter:added))))
   `(git-gutter+-deleted ((,class (:inherit git-gutter:deleted))))
   `(git-gutter+-modified ((,class (:inherit git-gutter:modified))))
   `(git-gutter-fr:added ((,class (:inherit git-gutter:added))))
   `(git-gutter-fr:deleted ((,class (:inherit git-gutter:deleted))))
   `(git-gutter-fr:modified ((,class (:inherit git-gutter:modified))))
   `(git-commit-summary ((,class (:foreground ,fg-bright))))
   `(git-commit-overlong-summary ((,class (:foreground ,salmon :weight bold))))
   `(git-commit-nonempty-second-line ((,class (:foreground ,gold))))
   `(git-commit-comment-heading ((,class (:foreground ,cerulean :weight bold))))
   `(git-commit-comment-branch-local ((,class (:foreground ,mint :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,cyan :weight bold))))
   `(git-commit-comment-file ((,class (:foreground ,lavender))))

   ;; highlight-symbol / hl-todo / hl-anything
   `(highlight-symbol-face ((,class (:background ,bg-match :underline (:color ,cyan)))))
   `(hl-todo ((,class (:foreground ,coral :weight bold))))
   `(hl-paren-face ((,class (:foreground ,peach :weight bold))))

   ;; hydra
   `(hydra-face-red ((,class (:foreground ,salmon :weight bold))))
   `(hydra-face-blue ((,class (:foreground ,cerulean :weight bold))))
   `(hydra-face-amaranth ((,class (:foreground ,coral :weight bold))))
   `(hydra-face-pink ((,class (:foreground ,orchid :weight bold))))
   `(hydra-face-teal ((,class (:foreground ,teal :weight bold))))

   ;; ivy & swiper
   `(ivy-current-match ((,class (:background ,bg-match :foreground ,fg-bright :weight bold :extend t))))
   `(ivy-minibuffer-match-face-1 ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(ivy-minibuffer-match-face-2 ((,class (:background ,bg-cyan :foreground ,cyan :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:background ,bg-coral :foreground ,coral :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:background ,bg-mint :foreground ,mint :weight bold))))
   `(ivy-minibuffer-match-highlight ((,class (:foreground ,teal :weight bold))))
   `(ivy-confirm-face ((,class (:foreground ,mint))))
   `(ivy-match-required-face ((,class (:foreground ,salmon))))
   `(ivy-virtual ((,class (:foreground ,comment :slant italic))))
   `(ivy-modified-buffer ((,class (:foreground ,peach :weight bold))))
   `(swiper-line-face ((,class (:background ,bg-region :foreground ,fg-bright :extend t))))
   `(swiper-match-face-1 ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(swiper-match-face-2 ((,class (:background ,bg-cyan-hi :foreground ,cyan :weight bold))))
   `(swiper-match-face-3 ((,class (:background ,bg-coral-hi :foreground ,coral :weight bold))))
   `(swiper-match-face-4 ((,class (:background ,bg-mint-hi :foreground ,mint :weight bold))))
   `(swiper-background-match-face-1 ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(swiper-background-match-face-2 ((,class (:background ,bg-cyan :foreground ,cyan))))
   `(swiper-background-match-face-3 ((,class (:background ,bg-coral :foreground ,coral))))
   `(swiper-background-match-face-4 ((,class (:background ,bg-mint :foreground ,mint))))

   ;; lsp-mode / lsp-ui
   `(lsp-face-highlight-textual ((,class (:background ,bg-hl :weight bold))))
   `(lsp-face-highlight-read ((,class (:background ,bg-cyan :underline (:color ,cyan)))))
   `(lsp-face-highlight-write ((,class (:background ,bg-peach :underline (:color ,peach)))))
   `(lsp-ui-doc-background ((,class (:background ,bg-popup))))
   `(lsp-ui-doc-header ((,class (:background ,bg-subtle :foreground ,fg-bright :weight bold))))
   `(lsp-ui-peek-filename ((,class (:foreground ,cyan :weight bold))))
   `(lsp-ui-peek-header ((,class (:background ,bg-subtle :foreground ,fg-bright :weight bold))))
   `(lsp-ui-peek-footer ((,class (:background ,bg-subtle))))
   `(lsp-ui-peek-selection ((,class (:background ,bg-match :foreground ,fg-bright :weight bold))))
   `(lsp-ui-peek-list ((,class (:background ,bg-popup))))
   `(lsp-ui-peek-peek ((,class (:background ,bg-popup))))
   `(lsp-ui-peek-highlight ((,class (:background ,bg-coral-hi :foreground ,coral :weight bold))))
   `(lsp-ui-peek-line-number ((,class (:foreground ,comment))))
   `(lsp-ui-sideline-global ((,class (:foreground ,comment))))

   ;; magit
   `(magit-bisect-bad ((,class (:foreground ,salmon))))
   `(magit-bisect-good ((,class (:foreground ,mint))))
   `(magit-bisect-skip ((,class (:foreground ,gold))))
   `(magit-blame-date ((,class (:foreground ,cerulean))))
   `(magit-blame-heading ((,class (:background ,bg-alt :foreground ,fg-alt :extend t))))
   `(magit-branch-current ((,class (:foreground ,coral :weight bold :box (:line-width -1 :color ,coral)))))
   `(magit-branch-local ((,class (:foreground ,cyan :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,mint :weight bold))))
   `(magit-cherry-equivalent ((,class (:foreground ,lavender))))
   `(magit-cherry-unmatched ((,class (:foreground ,cyan))))
   `(magit-diff-added ((,class (:background ,bg-mint :foreground ,mint :extend t))))
   `(magit-diff-added-highlight ((,class (:background ,bg-mint-hi :foreground ,fg-bright :extend t))))
   `(magit-diff-removed ((,class (:background ,bg-salmon :foreground ,salmon :extend t))))
   `(magit-diff-removed-highlight ((,class (:background ,bg-salmon-hi :foreground ,fg-bright :extend t))))
   `(magit-diff-base ((,class (:background ,bg-gold :foreground ,gold :extend t))))
   `(magit-diff-base-highlight ((,class (:background ,bg-gold-hi :foreground ,fg-bright :extend t))))
   `(magit-diff-context ((,class (:foreground ,comment))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt :foreground ,fg-alt :extend t))))
   `(magit-diff-file-heading ((,class (:foreground ,fg-bright :weight bold :extend t))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg-hl :foreground ,fg-bright :weight bold :extend t))))
   `(magit-diff-file-heading-selection ((,class (:background ,bg-match :foreground ,cyan :weight bold :extend t))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-subtle :foreground ,fg-alt :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-lavender-hi :foreground ,lavender :weight bold :extend t))))
   `(magit-diff-hunk-heading-selection ((,class (:background ,bg-match :foreground ,cyan :weight bold :extend t))))
   `(magit-diff-lines-boundary ((,class (:background ,coral))))
   `(magit-diff-lines-heading ((,class (:background ,coral :foreground ,bg-deep :weight bold))))
   `(magit-diffstat-added ((,class (:foreground ,mint))))
   `(magit-diffstat-removed ((,class (:foreground ,salmon))))
   `(magit-dimmed ((,class (:foreground ,comment))))
   `(magit-filename ((,class (:foreground ,fg-alt))))
   `(magit-hash ((,class (:foreground ,comment))))
   `(magit-header-line ((,class (:background ,bg-subtle :foreground ,coral :weight bold))))
   `(magit-log-author ((,class (:foreground ,peach))))
   `(magit-log-date ((,class (:foreground ,cerulean))))
   `(magit-log-graph ((,class (:foreground ,comment))))
   `(magit-process-ng ((,class (:inherit error))))
   `(magit-process-ok ((,class (:inherit success))))
   `(magit-reflog-amend ((,class (:foreground ,lavender))))
   `(magit-reflog-checkout ((,class (:foreground ,cerulean))))
   `(magit-reflog-cherry-pick ((,class (:foreground ,mint))))
   `(magit-reflog-commit ((,class (:foreground ,mint))))
   `(magit-reflog-merge ((,class (:foreground ,teal))))
   `(magit-reflog-other ((,class (:foreground ,cyan))))
   `(magit-reflog-rebase ((,class (:foreground ,orchid))))
   `(magit-reflog-remote ((,class (:foreground ,cyan))))
   `(magit-reflog-reset ((,class (:foreground ,salmon))))
   `(magit-refname ((,class (:foreground ,fg-dim))))
   `(magit-section-heading ((,class (:foreground ,coral :weight bold))))
   `(magit-section-heading-selection ((,class (:foreground ,peach :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-hl :extend t))))
   `(magit-section-secondary-heading ((,class (:foreground ,lavender :weight bold))))
   `(magit-sequence-drop ((,class (:foreground ,salmon))))
   `(magit-sequence-head ((,class (:foreground ,cerulean))))
   `(magit-sequence-part ((,class (:foreground ,gold))))
   `(magit-sequence-stop ((,class (:foreground ,mint))))
   `(magit-tag ((,class (:foreground ,gold))))

   ;; marginalia
   `(marginalia-key ((,class (:foreground ,teal))))
   `(marginalia-value ((,class (:foreground ,fg-dim))))
   `(marginalia-size ((,class (:foreground ,lavender))))
   `(marginalia-date ((,class (:foreground ,comment))))
   `(marginalia-documentation ((,class (:inherit font-lock-doc-face))))
   `(marginalia-file-name ((,class (:foreground ,cerulean))))
   `(marginalia-file-priv-dir ((,class (:foreground ,cerulean))))
   `(marginalia-file-priv-exec ((,class (:foreground ,mint))))
   `(marginalia-file-priv-link ((,class (:foreground ,cyan))))
   `(marginalia-file-priv-read ((,class (:foreground ,gold))))
   `(marginalia-file-priv-write ((,class (:foreground ,coral))))

   ;; markdown-mode
   `(markdown-header-face ((,class (:weight bold))))
   `(markdown-header-face-1 ((,class (:foreground ,cyan :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,coral :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,mint :weight bold))))
   `(markdown-header-face-4 ((,class (:foreground ,lavender :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,peach :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,gold :weight bold))))
   `(markdown-code-face ((,class (:background ,bg-deep :extend t))))
   `(markdown-inline-code-face ((,class (:background ,bg-alt :foreground ,peach))))
   `(markdown-pre-face ((,class (:background ,bg-deep :foreground ,teal))))
   `(markdown-list-face ((,class (:foreground ,coral))))
   `(markdown-blockquote-face ((,class (:foreground ,fg-alt :slant italic))))
   `(markdown-link-face ((,class (:foreground ,cyan))))
   `(markdown-reference-face ((,class (:foreground ,cerulean))))
   `(markdown-url-face ((,class (:foreground ,gold :underline t))))
   `(markdown-link-title-face ((,class (:foreground ,coral))))
   `(markdown-comment-face ((,class (:inherit font-lock-comment-face))))
   `(markdown-math-face ((,class (:foreground ,lavender :slant italic))))
   `(markdown-italic-face ((,class (:foreground ,teal :slant italic))))
   `(markdown-bold-face ((,class (:foreground ,gold :weight bold))))

   ;; nerd-icons
   `(nerd-icons-red ((,class (:foreground ,salmon))))
   `(nerd-icons-lred ((,class (:foreground ,coral-bright))))
   `(nerd-icons-dred ((,class (:foreground ,coral))))
   `(nerd-icons-green ((,class (:foreground ,mint))))
   `(nerd-icons-lgreen ((,class (:foreground ,mint-soft))))
   `(nerd-icons-dgreen ((,class (:foreground ,teal))))
   `(nerd-icons-yellow ((,class (:foreground ,gold))))
   `(nerd-icons-lyellow ((,class (:foreground ,gold-bright))))
   `(nerd-icons-dyellow ((,class (:foreground ,olive-gold))))
   `(nerd-icons-orange ((,class (:foreground ,peach))))
   `(nerd-icons-lorange ((,class (:foreground ,peach))))
   `(nerd-icons-dorange ((,class (:foreground ,peach))))
   `(nerd-icons-blue ((,class (:foreground ,cerulean))))
   `(nerd-icons-lblue ((,class (:foreground ,cyan))))
   `(nerd-icons-dblue ((,class (:foreground ,cerulean))))
   `(nerd-icons-blue-alt ((,class (:foreground ,teal))))
   `(nerd-icons-cyan ((,class (:foreground ,cyan))))
   `(nerd-icons-lcyan ((,class (:foreground ,cyan))))
   `(nerd-icons-dcyan ((,class (:foreground ,teal))))
   `(nerd-icons-purple ((,class (:foreground ,lavender))))
   `(nerd-icons-lpurple ((,class (:foreground ,lavender))))
   `(nerd-icons-dpurple ((,class (:foreground ,orchid))))
   `(nerd-icons-pink ((,class (:foreground ,coral))))
   `(nerd-icons-lpink ((,class (:foreground ,coral-bright))))
   `(nerd-icons-dpink ((,class (:foreground ,orchid))))
   `(nerd-icons-maroon ((,class (:foreground ,coral))))
   `(nerd-icons-lmaroon ((,class (:foreground ,coral-bright))))
   `(nerd-icons-dmaroon ((,class (:foreground ,orchid))))
   `(nerd-icons-silver ((,class (:foreground ,fg-alt))))
   `(nerd-icons-lsilver ((,class (:foreground ,fg))))
   `(nerd-icons-dsilver ((,class (:foreground ,comment))))

   ;; orderless
   `(orderless-match-face-0 ((,class (:background ,bg-cyan :foreground ,cyan :weight bold))))
   `(orderless-match-face-1 ((,class (:background ,bg-coral :foreground ,coral :weight bold))))
   `(orderless-match-face-2 ((,class (:background ,bg-mint :foreground ,mint :weight bold))))
   `(orderless-match-face-3 ((,class (:background ,bg-gold :foreground ,gold :weight bold))))

   ;; org-modern (avoid overriding `org-modern-label' width/height)
   `(org-modern-block-name ((,class (:foreground ,fg-dim))))
   `(org-modern-tag ((,class (:inherit org-modern-label :background ,bg-subtle :foreground ,fg-alt))))
   `(org-modern-done ((,class (:inherit org-modern-label :background ,bg-mint :foreground ,mint :weight semi-bold))))
   `(org-modern-todo ((,class (:inherit (org-todo org-modern-label) :weight semi-bold :inverse-video t))))
   `(org-modern-priority ((,class (:inherit (org-priority org-modern-label) :weight semi-bold :inverse-video t))))
   `(org-modern-date-active ((,class (:inherit org-modern-label :background ,bg-cyan :foreground ,cyan))))
   `(org-modern-time-active ((,class (:inherit org-modern-label :background ,cyan :foreground ,bg-deep :distant-foreground ,fg-bright :weight semi-bold))))
   `(org-modern-date-inactive ((,class (:inherit org-modern-label :background ,bg-alt :foreground ,fg-dim))))
   `(org-modern-time-inactive ((,class (:inherit org-modern-label :background ,bg-subtle :foreground ,fg-alt :distant-foreground ,fg-bright))))
   `(org-modern-horizontal-rule ((,class (:underline ,border :extend t))))
   `(org-modern-progress-complete ((,class (:background ,teal :foreground ,bg-deep :weight bold))))
   `(org-modern-progress-incomplete ((,class (:background ,bg-subtle :foreground ,fg-dim))))
   `(org-modern-internal-target ((,class (:inherit org-modern-done))))
   `(org-modern-radio-target ((,class (:inherit org-modern-done))))

   ;; popup
   `(popup-face ((,class (:background ,bg-popup :foreground ,fg))))
   `(popup-tip-face ((,class (:inherit tooltip))))
   `(popup-selection-face ((,class (:background ,bg-match :foreground ,fg-bright))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,coral))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,gold))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,mint))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,lavender))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,peach))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,teal))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,cerulean))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,orchid))))
   `(rainbow-delimiters-unmatched-face ((,class (:background ,bg-salmon-hi :foreground ,salmon :weight ultra-bold))))
   `(rainbow-delimiters-mismatched-face ((,class (:inherit rainbow-delimiters-unmatched-face))))

   ;; solaire-mode
   `(solaire-default-face ((,class (:inherit default :background ,bg-alt))))
   `(solaire-hl-line-face ((,class (:inherit hl-line :background ,bg-subtle))))
   `(solaire-fringe-face ((,class (:inherit solaire-default-face :foreground ,muted))))
   `(solaire-mode-line-face ((,class (:inherit mode-line))))
   `(solaire-mode-line-inactive-face ((,class (:inherit mode-line-inactive))))

   ;; transient
   `(transient-heading ((,class (:foreground ,fg-bright :weight bold))))
   `(transient-key ((,class (:foreground ,cyan :weight bold))))
   `(transient-key-exit ((,class (:foreground ,coral :weight bold))))
   `(transient-key-stay ((,class (:foreground ,mint :weight bold))))
   `(transient-key-return ((,class (:foreground ,gold :weight bold))))
   `(transient-argument ((,class (:foreground ,mint :weight bold))))
   `(transient-value ((,class (:foreground ,gold))))
   `(transient-inactive-value ((,class (:foreground ,comment))))
   `(transient-inactive-argument ((,class (:foreground ,comment))))
   `(transient-enabled-suffix ((,class (:background ,bg-mint :foreground ,mint :weight bold))))
   `(transient-disabled-suffix ((,class (:background ,bg-salmon :foreground ,salmon :weight bold))))
   `(transient-blue ((,class (:foreground ,cerulean))))
   `(transient-pink ((,class (:foreground ,coral))))
   `(transient-purple ((,class (:foreground ,lavender))))
   `(transient-red ((,class (:foreground ,salmon))))
   `(transient-teal ((,class (:foreground ,teal))))

   ;; treemacs
   `(treemacs-root-face ((,class (:foreground ,coral :weight bold))))
   `(treemacs-file-face ((,class (:foreground ,fg))))
   `(treemacs-directory-face ((,class (:foreground ,cerulean))))
   `(treemacs-directory-collapsed-face ((,class (:foreground ,cerulean))))
   `(treemacs-tags-face ((,class (:foreground ,cyan))))
   `(treemacs-fringe-indicator-face ((,class (:foreground ,coral))))
   `(treemacs-git-unmodified-face ((,class (:foreground ,fg))))
   `(treemacs-git-modified-face ((,class (:foreground ,peach))))
   `(treemacs-git-added-face ((,class (:foreground ,mint))))
   `(treemacs-git-renamed-face ((,class (:foreground ,lavender))))
   `(treemacs-git-ignored-face ((,class (:foreground ,comment))))
   `(treemacs-git-untracked-face ((,class (:foreground ,gold))))
   `(treemacs-git-conflict-face ((,class (:foreground ,salmon :weight bold))))
   `(treemacs-on-failure-pulse-face ((,class (:background ,bg-salmon-hi :foreground ,fg-bright :extend t))))
   `(treemacs-on-success-pulse-face ((,class (:background ,bg-mint-hi :foreground ,fg-bright :extend t))))
   `(treemacs-nerd-icons-file-face ((,class (:foreground ,fg-dim))))
   `(treemacs-nerd-icons-root-face ((,class (:foreground ,coral :weight bold))))

   ;; undo-tree & vundo
   `(undo-tree-visualizer-default-face ((,class (:foreground ,comment))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,mint :weight bold))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,cyan))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,coral))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,gold))))
   `(vundo-default ((,class (:background ,bg-alt :foreground ,fg))))
   `(vundo-stem ((,class (:foreground ,muted))))
   `(vundo-node ((,class (:foreground ,fg-dim))))
   `(vundo-highlight ((,class (:foreground ,coral :weight bold))))
   `(vundo-saved ((,class (:foreground ,mint :weight bold))))
   `(vundo-last-saved ((,class (:foreground ,cyan :weight bold))))

   ;; vertico
   `(vertico-current ((,class (:background ,bg-match :foreground ,fg-bright :weight semi-bold :extend t))))
   `(vertico-multiline ((,class (:foreground ,fg-dim))))
   `(vertico-group-title ((,class (:foreground ,cerulean :slant italic))))
   `(vertico-group-separator ((,class (:foreground ,border :strike-through t))))
   `(vertico-indexed ((,class (:foreground ,peach :weight bold))))
   `(vertico-mouse ((,class (:background ,bg-region))))
   `(vertico-quick1 ((,class (:background ,coral :foreground ,bg-deep :weight bold))))
   `(vertico-quick2 ((,class (:background ,cyan :foreground ,bg-deep :weight bold))))

   ;; web-mode & css
   `(css-selector ((,class (:foreground ,coral))))
   `(css-property ((,class (:foreground ,cyan))))
   `(css-proprietary-property ((,class (:foreground ,peach))))
   `(web-mode-doctype-face ((,class (:foreground ,comment))))
   `(web-mode-html-tag-face ((,class (:foreground ,coral))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,fg-dim))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,mint))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,gold))))
   `(web-mode-html-entity-face ((,class (:foreground ,lavender :slant italic))))
   `(web-mode-css-property-name-face ((,class (:foreground ,cyan))))
   `(web-mode-json-key-face ((,class (:foreground ,cerulean-dim))))
   `(web-mode-json-context-face ((,class (:foreground ,cerulean-dim))))
   `(web-mode-keyword-face ((,class (:inherit font-lock-keyword-face))))
   `(web-mode-string-face ((,class (:inherit font-lock-string-face))))
   `(web-mode-type-face ((,class (:inherit font-lock-type-face))))
   `(web-mode-block-control-face ((,class (:foreground ,peach))))
   `(web-mode-block-delimiter-face ((,class (:foreground ,peach))))

   ;; which-func & which-key
   `(which-func ((,class (:foreground ,mint))))
   `(which-key-key-face ((,class (:foreground ,coral :weight bold))))
   `(which-key-separator-face ((,class (:foreground ,comment))))
   `(which-key-note-face ((,class (:foreground ,fg-dim))))
   `(which-key-group-description-face ((,class (:foreground ,lavender))))
   `(which-key-command-description-face ((,class (:foreground ,cyan))))
   `(which-key-local-map-description-face ((,class (:foreground ,mint))))

   ;; yasnippet
   `(yas-field-highlight-face ((,class (:background ,bg-lavender :foreground ,fg-bright)))))

  ;; ------------------------------------------------------------------------
  ;; Theme Variables
  ;; ------------------------------------------------------------------------
  (custom-theme-set-variables
   'twilight-horizon
   `(ansi-color-names-vector
     [,bg-deep ,salmon ,mint ,gold ,cerulean ,coral ,cyan ,fg])
   `(vc-annotate-background ,bg)
   `(vc-annotate-very-old-color ,comment)
   `(vc-annotate-color-map
     '((20  . ,mint)
       (40  . ,teal)
       (60  . ,cyan)
       (80  . ,cerulean)
       (100 . ,gold)
       (140 . ,peach)
       (180 . ,coral)
       (220 . ,salmon)
       (260 . ,orchid)
       (300 . ,lavender)
       (340 . ,comment)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'twilight-horizon)
(provide 'twilight-horizon-theme)

;;; twilight-horizon-theme.el ends here

