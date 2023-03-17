;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREFERENCES

;; resize initial frame to 1/2 screen width
;; temporary ugly hack. it works for me...
(set-frame-width nil (if (= (default-font-width) 11) 162 146))
(set-frame-parameter nil 'energos/width 9)

(desktop-save-mode)                     ; restore desktop, except
(setq desktop-restore-frames nil)       ; for window and frame configuration
(savehist-mode)                         ; preserve minibuffer history
(recentf-mode)                          ; show recent files
(save-place-mode)                       ; remember last cursor position

(setq inhibit-startup-screen t)         ; disable 'splash screen'
(setq initial-buffer-choice t)          ; show *scratch* buffer at startup

(scroll-bar-mode -1)                    ; disable 'scrollbar'
(menu-bar-mode -1)                      ; disable 'menu'
(tool-bar-mode -1)                      ; disable 'tool-bar'
(tooltip-mode -1)                       ; disable 'tooltips'
(blink-cursor-mode -1)                  ; NON blinking cursor
(setq visible-cursor nil)               ; less annoying console cursor
(line-number-mode)                      ; display line number in 'mode-line'
(column-number-mode)                    ; display column number in 'mode-line'
(show-paren-mode)                       ; visualize matching parens

(setq frame-resize-pixelwise nil)       ; whole character frame resize
(setq-default truncate-lines nil)       ; wrap lines longer than the window width
(setq truncate-partial-width-windows 40)        ; except in "narrow" windows

(setq save-interprogram-paste-before-kill t)    ; ???
(delete-selection-mode)                 ; replace selection with typed text

(winner-mode)                           ; undo/redo window configuration

(prefer-coding-system 'utf-8-unix)      ; UTF-8, no crlf, please
(defalias 'yes-or-no-p 'y-or-n-p)       ; ask for "y or n"
(setq confirm-nonexistent-file-or-buffer t)     ; confirm file/buffer creation
(setq large-file-warning-threshold 100000000)   ; big file warning

(setq scroll-step 1)                    ; one line vertical scroll
(setq scroll-preserve-screen-position t); keep cursor position on PgUp/PgDown,
(setq scroll-error-top-bottom t)        ; except on first and last pages
(setq hscroll-step 1)                   ; one character horizontal scroll,
(setq hscroll-margin 0)                 ; on window edges

;; ediff: no control frame, side by side windows
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; prefer horizontal split (side-by-side)
(setq split-width-threshold 128)
(setq split-height-threshold nil)

;; --- Enable some disabled commands ---
;; https://www.emacswiki.org/emacs/DisabledCommands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; --- Do not expire passwords - DANGER! ---
(setq password-cache-expiry nil)
;; --- Do not save passwords ---
(setq auth-source-save-behavior nil)

;; --- Freaking TAB behaviour
(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method nil)
(electric-indent-mode -1)

(when (eq system-type 'windows-nt)
  ;; --- Tramp needs PuTTY on Windows® version of Emacs ---
  ;; ssh method works fine on Cygwin© and *nix Emacs
  ;; see https://www.gnu.org/software/tramp/
  (setq tramp-default-method "plink")
  ;; no Windows® sounds
  (setq ring-bell-function 'ignore))

;; --- Initial message ---
(let ((command "fortune"))
  (if (executable-find command)
      (setq initial-scratch-message
            (let ((string "")
                  (list (split-string (shell-command-to-string command) "\n")))
              (dolist (line list)
                (setq string (concat string
                                     ";;"
                                     (if (= 0 (length line)) "" "  ")
                                     line
                                     "\n")))
              string))))

;; --- webjump ---
(setq webjump-sites
      '(
        ("Emacs Home Page"      . "https://www.gnu.org/software/emacs/")
        ("Awesome Emacs"        . "https://github.com/emacs-tw/awesome-emacs")
        ("Emacs Wiki"           . "https://www.emacswiki.org")
        ("The other Emacs Wiki" . "https://wikemacs.org")
        ("It's Magit!"          . "https://magit.vc/")
        ("Gentoo Home Page"     . "https://www.gentoo.org/")
        ("Gentoo Packages"      . "https://packages.gentoo.org/")
        ("Gentoo app-emacs"     . "https://packages.gentoo.org/categories/app-emacs")
        ("Common Lisp"          . "https://lisp-lang.org/")
        ("Awesome Common Lisp"  . "https://github.com/CodyReichert/awesome-cl")
        ))
(global-set-key (kbd "H-w") 'webjump)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL elisp FILES

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; framemove
;; https://github.com/emacsmirror/framemove
(unless (string= (getenv "WINDOW_MANAGER") "emacs")
  (require 'framemove)
  (setq framemove-hook-into-windmove t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES

(require 'package)
(add-to-list 'package-archives
             '("melpa-stbl" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stbl" .  0)
        ("gnu"        .  5)
        ("melpa"      . 10)))
(package-initialize)

;; use-package
;; https://github.com/jwiegley/use-package
;; https://packages.gentoo.org/packages/app-emacs/use-package
(unless (require 'use-package nil t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; which-key
;; https://github.com/justbur/emacs-which-key
;; https://packages.gentoo.org/packages/app-emacs/which-key
(unless (require 'which-key nil t)
  (use-package which-key))
(setq which-key-idle-delay 3.0)
(which-key-mode)

;; It's Magit!
;; https://magit.vc/
;; https://github.com/magit/magit
;; https://packages.gentoo.org/packages/app-emacs/magit
(unless (require 'magit nil t)
  (use-package magit))
(bind-keys ("C-x g"         . magit-status)
           ("H-m"           . magit-status)
           :map magit-hunk-section-map
           ("<return>"      . magit-diff-visit-file-other-window)
           ("<S-return>"    . magit-diff-visit-file)
           :map magit-mode-map
           ("<M-tab>"       . nil)
           :map magit-section-mode-map
           ("<M-tab>"       . nil))
;; Syntax highlight for git commit messages
(require 'git-commit)
;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
(setq auto-revert-check-vc-info t)

;; minions
;; https://github.com/tarsius/minions
(use-package minions
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode))

;; ;; treemacs
;; ;; https://github.com/Alexander-Miller/treemacs
;; ;; useful ??
;; (use-package treemacs
;;   :bind (("<f8>"    . treemacs-select-window)
;;          ("C-<f8>"  . treemacs))
;;   :config
;;   (defun treemacs-custom-filter (file _)
;;     (or (s-ends-with? ".o"   file)
;;         (s-ends-with? ".map" file)
;;         (s-ends-with? ".log" file)
;;         (s-ends-with? ".elf" file)
;;         (s-ends-with? ".bin" file)
;;         (s-ends-with? ".sym" file)
;;         (s-equals? "GPATH" file)
;;         (s-equals? "GRTAGS" file)
;;         (s-equals? "GTAGS" file)))
;;   (push #'treemacs-custom-filter treemacs-ignored-file-predicates)
;;   (setq treemacs-width 25
;;         treemacs-is-never-other-window t))

;; ;; function-args
;; ;; https://github.com/abo-abo/function-args
;; ;; useful for company ??
;; (use-package function-args
;;   :config
;;   (fa-config-default))

;; flycheck
;; https://www.flycheck.org
;; https://github.com/flycheck/flycheck
;; https://packages.gentoo.org/packages/app-emacs/flycheck
(unless (require 'flycheck nil t)
  (use-package flycheck))

;; expand-region
;; https://github.com/magnars/expand-region.el
;; https://packages.gentoo.org/packages/app-emacs/expand-region
(unless (require 'expand-region nil t)
  (use-package expand-region))
(bind-keys ("C-=" . er/expand-region))

;; vertico
;; https://github.com/minad/vertico
;; https://packages.gentoo.org/packages/app-emacs/vertico
(unless (require 'vertico nil t)
  (use-package vertico))
(vertico-mode)
;; Different scroll margin
;; (setq vertico-scroll-margin 0)
(setq vertico-count 20)               ; Show more candidates
;; Grow and shrink the Vertico minibuffer
;; (setq vertico-resize t)
;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;; (setq vertico-cycle t)

;; orderless
;; https://github.com/oantolin/orderless
;; https://packages.gentoo.org/packages/app-emacs/orderless
(unless (require 'orderless nil t)
  (use-package orderless))
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; marginalia
;; https://github.com/minad/marginalia
;; https://packages.gentoo.org/packages/app-emacs/marginalia
(unless (require 'marginalia nil t)
  (use-package marginalia))
(marginalia-mode)

;; consult
;; https://github.com/minad/consult
;; https://packages.gentoo.org/packages/app-emacs/consult
(unless (require 'consult nil t)
  (use-package consult))
(bind-keys ("M-i"     . consult-line)
           ("M-y"     . consult-yank-from-kill-ring)
           ("C-x C-b" . switch-to-buffer)
           ("C-H-b"   . switch-to-buffer)
           ("C-x b"   . consult-buffer)
           ("H-b"     . consult-buffer))
(consult-customize
 consult-buffer :preview-key nil)

;; Shorten recent files in consult-buffer
;; https://github.com/minad/consult/wiki#shorten-recent-files-in-consult-buffer

(defun my-consult--source-recentf-items-uniq ()
  (let ((ht (consult--buffer-file-hash))
        file-name-handler-alist ;; No Tramp slowdown please.
        items)
    (dolist (file (my-recentf-list-uniq) (nreverse items))
      ;; Emacs 29 abbreviates file paths by default, see
      ;; `recentf-filename-handlers'.
      (unless (eq (aref (cdr file) 0) ?/)
        (setcdr file (expand-file-name (cdr file))))
      (unless (gethash (cdr file) ht)
        (push (propertize
               (car file)
               'multi-category `(file . ,(cdr file)))
              items)))))

(plist-put consult--source-recent-file
           :items #'my-consult--source-recentf-items-uniq)

(defun my-recentf-list-uniq ()
  (let* ((proposed (mapcar (lambda (f)
                             (cons (file-name-nondirectory f) f))
                           recentf-list))
         (recentf-uniq proposed)
         conflicts resol file)
    ;; collect conflicts
    (while proposed
      (setq file (pop proposed))
      (if (assoc (car file) conflicts)
          (push (cdr file) (cdr (assoc (car file) conflicts)))
        (if (assoc (car file) proposed)
            (push (list (car file) (cdr file)) conflicts))))
    ;; resolve conflicts
    (dolist (name conflicts)
      (let* ((files (mapcar (lambda (f)
                              ;; (file remaining-path curr-propos)
                              (list f
                                    (file-name-directory f)
                                    (file-name-nondirectory f)))
                            (cdr name)))
             (curr-step (mapcar (lambda (f)
                                  (file-name-nondirectory
                                   (directory-file-name (cadr f))))
                                files)))
        ;; Quick check, if there are no duplicates, we are done.
        (if (eq (length curr-step) (length (delete-dups curr-step)))
            (setq resol
                  (append resol
                          (mapcar (lambda (f)
                                    (cons (car f)
                                          (file-name-concat
                                           (file-name-nondirectory
                                            (directory-file-name (cadr f)))
                                           (file-name-nondirectory (car f)))))
                                  files)))
          (while files
            (let (files-remain)
              (dolist (file files)
                (let ((curr-propos (caddr file))
                      (curr-part (file-name-nondirectory
                                  (directory-file-name (cadr file))))
                      (rest-path (file-name-directory
                                  (directory-file-name (cadr file))))
                      (curr-step
                       (mapcar (lambda (f)
                                 (file-name-nondirectory
                                  (directory-file-name (cadr f))))
                               files)))
                  (if (member curr-part (cdr (member curr-part curr-step)))
                      ;; There is more than one curr-part in curr-step for
                      ;; this candidate.
                      (push (list (car file)
                                  rest-path
                                  (file-name-concat curr-part curr-propos))
                            files-remain)
                    ;; There is no repetition of curr-part in curr-step
                    ;; for this candidate.
                    (push (cons (car file)
                                (file-name-concat curr-part curr-propos))
                          resol))))
              (setq files files-remain))))))
    ;; apply resolved conflicts
    (let (items)
      (dolist (file recentf-uniq (nreverse items))
        (let ((curr-resol (assoc (cdr file) resol)))
          (if curr-resol
              (push (cons (cdr curr-resol) (cdr file)) items)
            (push file items)))))))

;; corfu
;; https://github.com/minad/corfu
;; https://github.com/minad/corfu/blob/main/extensions/corfu-popupinfo.el
;; https://packages.gentoo.org/packages/app-emacs/corfu
(unless (require 'corfu nil t)
  (use-package corfu))

;; Optional customizations
(setq
 ;;  corfu-cycle t                 ;; Enable cycling for `corfu-next/previous'
 ;;  corfu-auto t                  ;; Enable auto completion
 ;;  corfu-separator ?\s           ;; Orderless field separator
 corfu-quit-at-boundary nil        ;; Never quit at completion boundary
 ;; corfu-quit-no-match nil        ;; Never quit, even if there is no match
 ;; corfu-preview-current nil      ;; Disable current candidate preview
 ;; corfu-preselect 'prompt        ;; Preselect the prompt
 ;; corfu-on-exact-match nil       ;; Configure handling of exact matches
 ;; corfu-scroll-margin 5          ;; Use scroll margin

 ;; ;; https://github.com/Gavinok/emacs.d/blob/main/init.el
 ;; corfu-cycle t                  ;; Allows cycling through candidates
 corfu-auto nil                    ;; Enable auto completion
 corfu-auto-prefix 3
 corfu-auto-delay 0.0
 ;; corfu-echo-documentation 0.25  ;; Enable documentation for completions
 ;; corfu-preview-current 'insert  ;; Do not preview current candidate
 ;; corfu-preselect-first nil
 ;; corfu-on-exact-match nil       ;; Don't auto expand tempel snippets
 )

;; Enable Corfu only for certain modes.
;; :hook ((prog-mode . corfu-mode)
;;        (shell-mode . corfu-mode)
;;        (eshell-mode . corfu-mode))

;; Recommended: Enable Corfu globally.
;; This is recommended since Dabbrev can be used globally (M-/).
;; See also `corfu-excluded-modes'.
(global-corfu-mode)

;; embark
;; https://github.com/oantolin/embark
(use-package embark
  :bind
  (("C-."   . embark-act)         ;; pick some comfortable binding
   ("H-."   . embark-act)
   ("C-;"   . embark-dwim)        ;; good alternative: M-.
   ("H-<f13>" . embark-dwim)
   ("C-h B" . embark-bindings))   ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; useful ??

;; ;; geiser
;; ;; http://www.nongnu.org/geiser/
;; (use-package geiser
;;   :init
;;   (setq geiser-repl-startup-time 20000)
;;   (setq geiser-repl-use-other-window nil)
;;   (setq geiser-active-implementations '(guile racket chicken))
;;   (setq geiser-default-implementation 'guile))

;; (global-set-key (kbd "H-d") 'geiser-doc-symbol-at-point)
;; (global-set-key (kbd "H-g")
;;                 (lambda () "Set C-x C-e to call geiser-eval-last-sexp."
;;                   (interactive)
;;                   (progn (local-set-key (kbd "C-x C-e") 'geiser-eval-last-sexp)
;;                          (message "C-x C-e will call geiser-eval-last-sexp"))))
;; (global-set-key (kbd "H-G")
;;                 (lambda () "Set C-x C-e to call the default command."
;;                   (interactive)
;;                   (progn (local-unset-key (kbd "C-x C-e"))
;;                          (message "C-x C-e reset to default"))))

;; ;; geiser-guile
;; (use-package geiser-guile)

;; ;; inf-ruby
;; ;; https://github.com/nonsequitur/inf-ruby
;; (use-package inf-ruby)

;; ;; lsp-mode
;; ;; https://github.com/emacs-lsp/lsp-mode
;; ;; https://emacs-lsp.github.io/lsp-mode/
;; ;; https://github.com/elixir-lsp/elixir-ls
;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   (add-to-list 'exec-path (expand-file-name "elixir-ls" user-emacs-directory))
;;   :hook ((elixir-mode . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands (lsp lsp-deferred))

;; ;; elixir-mode
;; ;; https://github.com/elixir-editors/emacs-elixir
;; (use-package elixir-mode)

;; ;; ob-elixir
;; ;; https://github.com/zweifisch/ob-elixir
;; (use-package ob-elixir)

;; ;; inf-elixir
;; ;; https://github.com/J3RN/inf-elixir
;; (use-package inf-elixir
;;   :after (elixir-mode))

;; htmlize
;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize)

;; pdf-tools
;; https://github.com/vedang/pdf-tools
(unless (require 'pdf-tools nil t)
  (use-package pdf-tools
    :demand t))
(bind-keys :map pdf-view-mode-map
           ("<home>"   . image-bob)
           ("<end>"    . image-eob)
           ("<C-home>" . pdf-view-first-page)
           ("<C-end>"  . pdf-view-last-page))
;; ugly hack...
(when (eq system-type 'berkeley-unix)
  (load "pdf-tools-init.el"))
(pdf-tools-install)
(setq-default pdf-view-display-size 'fit-page)
(setq pdf-view-midnight-colors '("#eaeaea" . "#181a26"))

;; ;; org-pdf-tool
;; ;; https://github.com/fuxialexander/org-pdftools
;; (use-package org-pdftools
;;   :after (pdf-tools)
;;   :hook (org-load . org-pdftools-setup-link))

;; ;; org-noter
;; ;; https://github.com/weirdNox/org-noter
;; (use-package org-noter
;;   :after (org)
;;   :config
;;   (setq org-noter-doc-split-fraction '(0.4 . 0.5)
;;         org-noter-default-notes-file-names '("books.org")
;;         org-noter-notes-search-path '("~/Work/org")))

;; TODO...
;;
;; https://github.com/fuxialexander/org-pdftools
;; org-noter-pdftools
;;
;; https://github.com/Kungsgeten/org-brain
;; org-brain
;;
;; https://github.com/org-roam/org-roam
;; org-roam

;; ace-window
;; https://github.com/abo-abo/ace-window
;; https://packages.gentoo.org/packages/app-emacs/ace-window
(unless (require 'ace-window nil t)
  (use-package ace-window))
(bind-keys ("H-o" . ace-window))
(setq aw-dispatch-always t)

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(require 'embark)
(eval-when-compile
  (defmacro embark-ace-action (fn)
    `(defun ,(intern (concat "embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(define-key embark-file-map     (kbd "o") (embark-ace-action find-file))
(define-key embark-buffer-map   (kbd "o") (embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o") (embark-ace-action bookmark-jump))

;; ;; projectile
;; ;; https://github.com/bbatsov/projectile
;; (use-package projectile
;;   :config
;;   (projectile-mode +1)
;;   (setq projectile-completion-system 'ivy)
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; ;; perspective
;; ;; https://github.com/nex3/perspective-el
;; (use-package perspective
;;   :config
;;   (persp-mode))
;; (use-package persp-projectile)

;; ;; gforth
;; ;; https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/Emacs-and-Gforth.html
;; ;; (autoload 'forth-mode "gforth.el")
;; (load "gforth.el")
;; (add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
;; (add-to-list 'auto-mode-alist '("\\.fth$" . forth-mode))

;; calibredb
;; https://github.com/chenyanming/calibredb.el
(use-package calibredb
  :demand t
  :config
  (require 'consult)
  (setq calibredb-root-dir "~/Library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Library")))
  (setq calibredb-date-width 0))

;; nix-mode
;; https://github.com/NixOS/nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")

;; vterm
;; https://github.com/akermu/emacs-libvterm
;; https://packages.gentoo.org/packages/app-emacs/vterm
(unless (require 'vterm nil t)
  (use-package vterm))
(setq vterm-min-window-width 54)
(setq vterm-clear-scrollback-when-clearing t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES

;; themes from the packages repository:
(use-package afternoon-theme    :defer t)
(use-package ample-zen-theme    :defer t)
(use-package blackboard-theme   :defer t)
(use-package darkmine-theme     :defer t)
(use-package darktooth-theme    :defer t)
(use-package eclipse-theme      :defer t)
(use-package hc-zenburn-theme   :defer t)
(use-package idea-darkula-theme :defer t)
(use-package lush-theme         :defer t)
(use-package material-theme     :defer t)
(use-package naquadah-theme     :defer t)
(use-package reverse-theme      :defer t)
(use-package tangotango-theme   :defer t)
(use-package zenburn-theme      :defer t)

;; hand picked themes:
;; https://github.com/emacs-jp/replace-colorthemes
;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; choose one of them:
;; (load-theme 'charcoal-black t t)
;; (enable-theme 'charcoal-black)
(load-theme 'afternoon t t)
(enable-theme 'afternoon)
(set-face-attribute 'highlight nil :background "#294F6E")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFUNs

(global-set-key (kbd "M-]") 'goto-match-paren)
;; https://www.emacswiki.org/emacs/NavigatingParentheses
(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis.
Else go to the opening parenthesis one level up."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

;; http://emacsblog.org/2007/01/17/indent-whole-buffer/
;; indent whole buffer, remove trailing spaces
(defun iwb ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun energos/inc-or-dec (n max &optional dec)
  "Increment or decrement N, limiting the result to the interval 0≤N≤MAX.
If DEC is t:             Return N-1 if 0<N≤MAX, 0 if N≤0, MAX if N>MAX.
If DEC is nil or absent: Return N+1 if 0≤N<MAX, 0 if N<0, MAX if N≥MAX."
  (or (integerp n) (setq n 0))                ; must be an integer
  (setq n (or (and dec (1- n)) (1+ n)))       ; increment or decrement
  (or (and (< n 0) 0) (and (> n max) max) n)) ; limit to the valid range

(defun energos/resize-frame (&optional dec)
  "If DEC is t, decrease current frame size, else increase current frame size."
  (interactive "P")
  (let* ((list11 [64 75 86 97 108 118 129 140 151 162 173 184 195 206 217 228 238 249 260 271 282 293 304 315 326 337 347])
         (list13 [54 63 72 82  91 100 109 119 128 137 146 156 165 174 183 192 202 211 220 230 239 248 257 266 276 285 294])
         (list (if (= (default-font-width) 11) list11 list13))
         (n (frame-parameter nil 'energos/width))
         (i (energos/inc-or-dec (if (integerp n) n 4) (1- (length list)) dec))
         (width (aref list i)))
    (set-frame-parameter nil 'energos/width i)
    (set-frame-width nil width)
    (message (format "Frame width resized to %d characters" width))))

;; usefull ??

;; ;; --- Send a key event to another application in same desktop ---
;; (defun send-key-to-window-class (class key &optional noactivate)
;;   "Send a KEY event to a application of class CLASS in same desktop.
;; If last argument NOACTIVATE (optional) is t, will send the key without
;; activating the window. Some applications will refuse to accept the key
;; if its window is not active."
;;   (if noactivate (setq noactivate "NOACTIVATE") (setq noactivate ""))
;;   (if (executable-find "xdotool")
;;       (message (shell-command-to-string (format "\
;; class=\"%s\"; key=\"%s\"; noact=\"%s\"; \
;; window=$(xdotool search --desktop $(xdotool get_desktop) --classname ^${class}$ | head -1); \
;; if \[\[ -n ${window} \]\]; then \
;; xdotool keyup ${key}; \
;; \[\[ -z ${noact} \]\] && actual=$(xdotool getwindowfocus); \
;; echo -n Sending key \\'${key}\\' to \\'${class}\\' window; \
;; \[\[ -z ${noact} \]\] && xdotool windowactivate --sync ${window} key ${key} \|\| \
;; xdotool key --window ${window} ${key}; \
;; \[\[ -z ${noact} \]\] && xdotool windowactivate ${actual}; \
;; else echo -n No \\'${class}\\' window found in current desktop; fi"
;;                                                 class key noactivate)))
;;     (message "No 'xdotool' executable found")))

;; ;; --- Recarregar o Browser ---
;; (defun browser-reload ()
;;   "Reload current desktop browser window."
;;   (interactive)
;;   (send-key-to-window-class "Navigator" "F5"))

;; --- Symbol highlighting ---
;; https://stackoverflow.com/questions/23891638/emacs-highlight-symbol-in-multiple-windows
(require 'hi-lock)
(defun unhighlight ()
  "Unhighlight all."
  (interactive)
  (unhighlight-regexp t))

(defun unhighlight-all-windows ()
  "Unhighlight all in all windows."
  (interactive)
  (save-selected-window
    (cl-dolist (x (window-list))
      (select-window x)
      (unhighlight-regexp t))))

(defun highlight-symbol ()
  "Highlight symbol at point."
  (interactive)
  (let* ((regexp (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp)))
         (hi-lock-auto-select-face t)
         (face (hi-lock-read-face-name)))
    (highlight-regexp regexp face)))

(defun unhighlight-symbol ()
  "Unhighlight symbol at point."
  (interactive)
  (let ((regexp (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp))))
    (unhighlight-regexp regexp)))

(defun highlight-symbol-all-windows ()
  "Highlight symbol at point in all windows."
  (interactive)
  (let* ((regexp (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp)))
         (hi-lock-auto-select-face t)
         (face (hi-lock-read-face-name)))
    (save-selected-window
      (cl-dolist (x (window-list))
        (select-window x)
        (highlight-regexp regexp face)))))

(defun unhighlight-symbol-all-windows ()
  "Highlight symbol at point in all windows."
  (interactive)
  (let ((regexp (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp))))
    (save-selected-window
      (cl-dolist (x (window-list))
        (select-window x)
        (unhighlight-regexp regexp)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE

;; TODO...

;; (setq org-directory "~/org")
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; (setq org-agenda-files '("~/org/notes.org" "~/org/agenda.org"))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((ditaa . nil)
;;    (dot . t)
;;    (calc . t)
;;    (shell . t)
;;    (emacs-lisp . t)
;;    (gnuplot . t)
;;    (ruby . t)
;;    (C . t)
;;    (python . t)
;;    (scheme . t)
;;    (forth . t)
;;    (elixir . t)
;;    ))

;; ;; --- DANGER! ---
;; ;; Avaliar código sem exigir confirmação
;; ;; https://orgmode.org/manual/Code-evaluation-security.html
;; ;; --- DANGER! ---
;; (setq org-confirm-elisp-link-function nil)
;; (setq org-confirm-babel-evaluate
;;       (lambda (lang body)
;;         (not (or
;;               ;; t                        ; não pergunto nada
;;               (string= lang "ditaa")      ; não pergunto por ditaa
;;               (string= lang "dot")        ; ...
;;               (string= lang "calc")
;;               (string= lang "emacs-lisp")
;;               (string= lang "elisp")
;;               (string= lang "C")
;;               (string= lang "C++")
;;               (string= lang "sh")         ; https://emacs.stackexchange.com/questions/35321/
;;               (string= lang "shell")      ;
;;               (string= lang "ruby")
;;               (string= lang "python")
;;               (string= lang "scheme")
;;               (string= lang "forth")
;;               (string= lang "elixir")
;;               ))))

;; ;; https://emacs.stackexchange.com/questions/2387/browser-not-opening-when-exporting-html-from-org-mode
;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.mm\\'" . default)
;;         ("\\.x?html?\\'" . "xdg-open %s")
;;         ("\\.djvu\\'" . "xdg-open %s")
;;         ("\\.pdf\\'" . "xdg-open %s")))

;; ;; --- preferences ---
(setq org-replace-disputed-keys t)
;; (setq org-src-fontify-natively t)
;; (setq org-src-tab-acts-natively t)
;; (setq org-link-frame-setup
;;       (quote
;;        ((vm . vm-visit-folder-other-frame)
;;         (vm-imap . vm-visit-imap-folder-other-frame)
;;         (gnus . org-gnus-no-new-news)
;;         (file . find-file)
;;         (wl . wl-other-frame))))
;; (setq org-goto-interface 'outline-path-completion
;;       org-goto-max-level 10)
;; (setq org-export-coding-system 'utf-8)

;; ;; --- org-refile ---
;; ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;; (setq org-refile-targets '((nil . (:maxlevel . 6)) (org-agenda-files . (:maxlevel . 6))))
;; (setq org-refile-use-outline-path 'file)
;; (setq org-outline-path-complete-in-steps nil)

;; ;; --- org-capture ---
;; (require 'org-protocol)
;; (require 'org-capture)

;; (setq org-capture-templates
;;       '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
;;          "* TODO %u %?" :prepend t)
;;         ("j" "Journal entry" entry (file+datetree "journal.org")
;;          "* %U %^{Title}\n  %?")
;;         ("n" "Note" entry (file+headline org-default-notes-file "Unsorted Notes")
;;          "* %?\n  %i\n  %a")
;;         ))
;; ;; See org-capture-templates help:
;; ;; %?          After completing the template, position cursor here.
;; ;;
;; ;; %i          Initial content, copied from the active region.  If %i is
;; ;;             indented, the entire inserted text will be indented as well.
;; ;;
;; ;; %a          Annotation, normally the link created with `org-store-link'.
;; ;; %t          Time stamp, date only.
;; ;; %T          Time stamp, with date and time.
;; ;; %u, %U      Like the above, but inactive time stamps.

;; ;; --- link to man pages ---
;; ;; https://www.gnu.org/software/emacs/manual/html_node/org/Adding-hyperlink-types.html
;; ;; org-man.el is archived in package org-plus-contrib at https://orgmode.org/elpa.html
;; (require 'org-man)
;; (setq org-man-command 'man)

;; ;; --- org-mode specific shortcuts ---
;; (global-set-key (kbd "C-c b") 'org-switchb)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)

;; --- restore old shortcuts for structure templates ---
;; https://orgmode.org/manual/Structure-Templates.html
(require 'org-tempo)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD SHORTCUTS

;; --- Use hippie-expand instead of dabbrev-expand ---
(global-set-key (kbd "M-/") 'hippie-expand)

;; --- We don't need a freaking mouse wheel ---
(mouse-wheel-mode -1)
(global-set-key (kbd "<mouse-4>") 'ignore)
(global-set-key (kbd "<mouse-5>") 'ignore)

;; --- Translate Caps_Lock (via xmodmap) -> F13 -> Hyper ---
(global-set-key (kbd "<f13>") 'ignore)
(define-key key-translation-map (kbd "<f13>") 'event-apply-hyper-modifier)
(when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
  (setq w32-enable-caps-lock nil)
  (define-key key-translation-map (kbd "<capslock>") 'event-apply-hyper-modifier)
  (message "Windows® system detected")
  )

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-<up>") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "C-<left>") (lambda () (interactive) (scroll-right 1)))
(global-set-key (kbd "C-<right>") (lambda () (interactive) (scroll-left 1)))

(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (scroll-other-window 1)))

(windmove-default-keybindings 'hyper)

(global-set-key (kbd "<f12>") 'toggle-truncate-lines)

(global-set-key (kbd "C-<prior>")
                (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-<next>")
                (lambda () (interactive) (move-to-window-line -1)))

(global-set-key (kbd "C-<tab>")
                (lambda (arg) "Insert TAB." (interactive "p") (insert-tab arg)))

;; https://www.emacswiki.org/emacs/SearchAtPoint#toc7
(global-set-key (kbd "M-b") (lambda (arg) "\
Move point to the previous position that is the beggining of a symbol."
                              (interactive "p") (forward-symbol (- arg))))
(global-set-key (kbd "M-f") 'forward-symbol)

(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)
(setq browse-url-browser-function 'browse-url-xdg-open)

(global-set-key (kbd "M-]") 'goto-match-paren)

;; --- Mostrar arquivo correspondente ao buffer ---
(global-set-key (kbd "S-<f12>")
                (lambda () "Show current buffer file path."
                  (interactive)
                  (or (message buffer-file-name)
                      (message "This buffer is not a file!"))))

;; --- Recarregar buffer ---
(global-set-key (kbd "<f5>")
                (lambda () "Reload buffer from disk if modified."
                  (interactive)
                  (revert-buffer t (not (buffer-modified-p)) t)))

;; --- Frame resize ---
(if (string= (getenv "WINDOW_MANAGER") "emacs")
    (global-unset-key (kbd "<f11>"))
  (global-set-key (kbd "<f11>") 'energos/resize-frame)
  (global-set-key (kbd "S-<f11>")
                  (lambda () (interactive) (energos/resize-frame t)))
  (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen))

;; --- Symbol highlighting ---
(global-set-key (kbd "H-H") 'highlight-symbol-all-windows)
(global-set-key (kbd "H-h") 'highlight-symbol)

(global-set-key (kbd "H-U") 'unhighlight-symbol-all-windows)
(global-set-key (kbd "H-u") 'unhighlight-symbol)

(global-set-key (kbd "H-A") 'unhighlight-all-windows)
(global-set-key (kbd "H-a") 'unhighlight)

;; --- Frequently used files ---
(global-set-key (kbd "\e\ei") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(global-set-key (kbd "\e\en") (lambda () (interactive) (find-file org-default-notes-file)))
(global-set-key (kbd "\e\es") (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; --- Insert key ---
;; I keep accidentally hitting that damn key
(global-set-key (kbd "<insert>") (lambda () (interactive) (overwrite-mode -1)))
(global-set-key (kbd "C-x <insert>") 'overwrite-mode)

;; --- Kill buffers ---
(global-set-key (kbd "C-x K") 'kill-buffer)
(global-set-key (kbd "H-K") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "H-k") 'kill-current-buffer)
;; Never kill *scratch* or *Messages*
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

;; --- find/save/exit ---
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-f") 'find-file)
(global-set-key (kbd "H-Q") 'save-buffers-kill-terminal)

;; -- frames/windows --
(unless (string= (getenv "WINDOW_MANAGER") "emacs")
  (global-set-key (kbd "H-n") 'make-frame-command))
(global-set-key (kbd "H-0")
                (lambda () "Delete window or delete frame if there is only one window."
                  (interactive)
                  (if (one-window-p)
                      (if (string= (getenv "WINDOW_MANAGER") "emacs")
                          (message "Refusing to delete the last window")
                        (delete-frame))
                    (delete-window))))
(global-set-key (kbd "H-1") 'delete-other-windows)
(global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-3") 'split-window-horizontally)

;; ???
(global-set-key (kbd "H-c") (kbd "C-c C-c"))
(global-set-key (kbd "H-e") (kbd "C-x C-e"))

;; ;; useful ??

;; ;; --- Send a key event to another application        ---
;; ;; --- WARNING! Send the same key used as a shortcut! ---
;; (global-set-key (kbd "H-<f5>") 'browser-reload)
;; (global-set-key (kbd "H-<prior>")
;;                 (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "Prior")))
;; (global-set-key (kbd "H-<next>")
;;                 (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "Next")))
;; (global-set-key (kbd "H-<home>")
;;                 (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "Home")))
;; (global-set-key (kbd "H-<end>")
;;                 (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "End")))

;; ;; --- Make ---
;; (global-set-key (kbd "<f9>") (lambda () "Make all" (interactive) (save-buffer 0) (compile "make")))
;; (global-set-key (kbd "S-<f9>") (lambda () "Make clean" (interactive) (save-buffer 0) (compile "make clean")))
;; (global-set-key (kbd "C-<f9>") (lambda () "Make flash" (interactive) (save-buffer 0) (compile "make flash")))
;; (global-set-key (kbd "C-S-<f9>") (lambda () "Make verify" (interactive) (compile "make verify")))
;; (global-set-key (kbd "M-<f9>") (lambda () "Make reset" (interactive) (compile "make reset")))

;; (setq compilation-window-height 24)
;; (setq compilation-scroll-output 'first-error)

;; ;; --- Support for keyboard physical macro keys ---
;; ;;
;; ;; M1 - ESC <f1> -> C-c &
;; ;; M2 - ESC <f2> -> C-c C-o
;; ;; M3 - ESC <f3> -> C-c C-c
;; ;; M4 - ESC <f4> -> C-x C-e
;; ;; M5 - ESC <f5> -> C-x

;; ;; m1 map
;; (define-prefix-command 'energos/m1-map)
;; (define-key energos/m1-map (kbd "<f1>") (kbd "C-x C-e"))
;; (define-key energos/m1-map (kbd "<f2>") (kbd "C-x C-e"))
;; (define-key energos/m1-map (kbd "<f3>") (kbd "C-x C-e"))
;; (define-key (current-global-map) (kbd "H-<escape>") energos/m1-map)

;; ;; M1
;; (define-key key-translation-map (kbd "ESC <f1>") (kbd "C-c &"))

;; ;; M2
;; (define-key key-translation-map (kbd "ESC <f2>") (kbd "C-c C-o"))

;; ;; M3
;; (define-key key-translation-map (kbd "ESC <f3>") (kbd "C-c C-c"))

;; ;; M4
;; (define-key key-translation-map (kbd "ESC <f4>") (kbd "C-x C-e"))

;; ;; M5
;; (define-key key-translation-map (kbd "ESC <f5>") (kbd "C-x"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AREA 51

(let ((file (expand-file-name "experimental.el" user-emacs-directory)))
  (when (file-exists-p file)
    (load file)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START SERVER!

(require 'server)
;; ~/bin/emacs-git needs to know which server to connect to
(setenv "EMACS_SERVER" server-name)
(unless (server-running-p)
  (server-start))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZE
;; Keep this file free from customize data

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
