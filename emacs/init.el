;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PREFERÊNCIAS
;;
(desktop-save-mode 1)                   ; restaurar desktop, exceto
(setq desktop-restore-frames nil)       ; a configuração de windows e frames

(setq inhibit-startup-screen t)         ; desabilitar 'splash screen'
(setq initial-buffer-choice t)          ; iniciar mostrando o *scratch* buffer

(scroll-bar-mode -1)                    ; desabilitar 'scrollbar'
(menu-bar-mode -1)                      ; desabilitar 'menu'
(tool-bar-mode -1)                      ; desabilitar 'tool-bar'
(tooltip-mode -1)                       ; desabilitar 'tooltips'
(blink-cursor-mode -1)                  ; cursor NÃO piscando
(setq visible-cursor nil)               ; cursor menos gritante no terminal (console)
(line-number-mode 1)                    ; mostrar linha no 'mode line'
(column-number-mode 1)                  ; mostra coluna no 'mode-line'
(show-paren-mode 1)                     ; destacar par de parenteses
(setq frame-resize-pixelwise t)         ; pixel perfect resize
(setq-default truncate-lines t)         ; disable line wrap
(setq truncate-partial-width-windows nil)
(setq save-interprogram-paste-before-kill t)
(delete-selection-mode 1)
(winner-mode 1)

(prefer-coding-system 'utf-8-unix)      ; UTF-8, no crlf, please

(setq scroll-step 1)                    ; scroll de apenas 1 linha
(setq scroll-preserve-screen-position t); PgUp/PgDown mantém posição do cursor,
(setq scroll-error-top-bottom t)        ; exceto na primeira e última página
(setq hscroll-step 1)                   ; scroll horizontal de 1 caracter,
(setq hscroll-margin 0)                 ; apenas ao chegar aos extremos da janela

(defalias 'yes-or-no-p 'y-or-n-p)       ; "y or n" em vez de "yes or no"
(setq confirm-nonexistent-file-or-buffer t) ; confirmar criação arquivo/buffer

;; ediff sem frame de controle e com janelas lado a lado
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; prefer horizontal split (side-by-side)
(setq split-width-threshold 132)
(setq split-height-threshold nil)

(defalias 'list-buffers 'ibuffer)

;; --- Enable some disabled commands ---
;; https://www.emacswiki.org/emacs/DisabledCommands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; --- Não expiro senhas - DANGER! ---
(setq password-cache-expiry nil)

;; --- Freaking TAB behaviour
(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode -1)
(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "<tab>") 'indent-for-tab-command)))

;; --- Mensagem inicial ---
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

;; --- Tramp needs PuTTY on Windows® version of Emacs ---
;; ssh method works fine on Cygwin© and *nix Emacs
;; see https://www.gnu.org/software/tramp/
(when (eq system-type 'windows-nt)
  (setq tramp-default-method "plink"))

;; --- webjump ---
(setq webjump-sites
      '(
        ("Google" .
         [simple-query "https://www.google.com"
                       "https://www.google.com/search?q="
                       ""])
        ("DuckDuckGo" .
         [simple-query "https://duckduckgo.com"
                       "https://duckduckgo.com/?q="
                       ""])
        ("The Weather Channel" .
         "https://weather.com/pt-BR/clima/10dias/l/BRXX0163:1:BR")
        ("Wikipedia" .
         [simple-query "https://wikipedia.org"
                       "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search="
                       ""])
        ("Oxford Dictionary" .
         [simple-query "https://en.oxforddictionaries.com"
                       "https://en.oxforddictionaries.com/definition/"
                       ""])
        ("Urban Dictionary" .
         [simple-query "https://www.urbandictionary.com"
                       "https://www.urbandictionary.com/define.php?term="
                       ""])
        ("Emacs Wiki" .
         [simple-query "https://www.emacswiki.org"
                       "https://www.emacswiki.org/cgi-bin/wiki/"
                       ""])
        ("Emacs Home Page" .
         "https://www.gnu.org/software/emacs/emacs.html")
        ))

(global-set-key (kbd "H-w") 'webjump)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL elisp FILES
;;
(add-to-list 'load-path "~/.emacs.d/lisp")

;; https://emacsmirror.net/

;; framemove
;; https://github.com/emacsmirror/framemove
(require 'cl)
(require 'framemove)
(setq framemove-hook-into-windmove t)

;; help-fns+
;; https://github.com/emacsmirror/help-fns-plus
(require 'help-fns+)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;
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

;; https://stackoverflow.com/questions/57153556/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 3.0)
  (which-key-mode))

;; https://github.com/magit/magit
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :bind (:map magit-hunk-section-map
              ("RET"        . magit-diff-visit-file-other-window)
              ("<S-return>" . magit-diff-visit-file))
  :init
  ;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
  ;; (setq vc-handled-backends nil)
  (setq auto-revert-check-vc-info t))

(use-package minions
  :ensure t
  :config
  (setq minions-mode-line-lighter "[+]")
  (minions-mode 1))

;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :bind (("C-x b"   . helm-buffers-list)
         ("C-x C-b" . helm-mini)
         ("M-x"     . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (setq helm-split-window-default-side 'other)
  (setq helm-candidate-number-limit 1000)
  (setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame t
        helm-use-undecorated-frame-option t
        helm-display-buffer-width 100
        helm-display-buffer-height 36)
  (customize-set-variable 'helm-ff-lynx-style-map t)
  (helm-mode 1))

;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure t
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (setq helm-swoop-split-window-function 'helm-display-buffer-in-own-frame)
  (require 'helm))

;; https://github.com/emacsorphanage/helm-gtags
;; https://tuhdo.github.io/c-ide.html
(use-package helm-gtags
  :ensure t
  :bind (:map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-j"     . helm-gtags-select)
              ("M-."     . helm-gtags-dwim)
              ("M-,"     . helm-gtags-pop-stack)
              ("C-c <"   . helm-gtags-previous-history)
              ("C-c >"   . helm-gtags-next-history))
  :hook ((c-mode c++-mode asm-mode) . helm-gtags-mode)
  :init
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)
  :config
  (require 'helm))

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :bind (("<f8>"    . treemacs-select-window)
         ("C-<f8>"  . treemacs))
  :config
  (defun treemacs-custom-filter (file _)
    (or (s-ends-with? ".o"   file)
        (s-ends-with? ".map" file)
        (s-ends-with? ".log" file)
        (s-ends-with? ".elf" file)
        (s-ends-with? ".bin" file)
        (s-ends-with? ".sym" file)
        (s-equals? "GPATH" file)
        (s-equals? "GRTAGS" file)
        (s-equals? "GTAGS" file)))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates)
  (setq treemacs-width 25
        treemacs-is-never-other-window t))

;; https://github.com/abo-abo/function-args
(use-package function-args
  :ensure t
  :config
  (fa-config-default))

;; http://company-mode.github.io/
(use-package company
  :ensure t
  :bind (:map company-mode-map
              ("<tab>" . company-indent-or-complete-common))
  :hook (c-mode . company-mode)
  :config
  (setq company-idle-delay 3))

;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-c-headers-path-system "/usr/avr/include")
  (add-to-list 'company-backends 'company-c-headers))

;; https://github.com/flycheck/flycheck
(use-package flycheck
  :ensure t)

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; http://www.nongnu.org/geiser/
(use-package geiser
  :ensure t
  :init
  (setq geiser-repl-startup-time 20000)
  (setq geiser-repl-use-other-window nil)
  (setq geiser-active-implementations '(guile racket chicken))
  (setq geiser-default-implementation 'guile))

;; https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t)

;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t)

;; https://github.com/hrs/engine-mode
(use-package engine-mode
  :ensure t
  :config
  (defengine google
    "https://www.google.com/search?q=%s"
    :keybinding "g")
  (defengine youtube
    "https://www.youtube.com/results?search_query=%s"
    :keybinding "y")
  (defengine dictionary
    "https://en.oxforddictionaries.com/definition/%s"
    :keybinding "d")
  (defengine urban-dictionary
    "https://www.urbandictionary.com/define.php?term=%s"
    :keybinding "u")
  (defengine kabum
    "https://www.kabum.com.br/cgi-local/site/listagem/listagem.cgi?string=%s"
    :keybinding "k")
  (defengine tweakers
    "https://tweakers.net/zoeken/?keyword=%s"
    :keybinding "t")
  (defengine hardware.info
    "https://nl.hardware.info/#search:%s"
    :keybinding "h")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")
  (defengine wikipedia
    "https://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (engine/set-keymap-prefix (kbd "H-/"))
  (global-set-key (kbd "H-t") 'engine/search-tweakers)
  (engine-mode t))

;; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-midnight-colors '("#eaeaea" . "#181a26"))
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  (define-key pdf-view-mode-map (kbd "<home>") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "<end>") 'pdf-view-last-page))

;; https://github.com/fuxialexander/org-pdftools
(use-package org-pdftools
  :ensure t
  :hook (org-load . org-pdftools-setup-link))

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :bind (("H-o" . ace-window)))

;; https://github.com/bbatsov/projectile
(use-package helm-projectile
  :ensure t)
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (helm-projectile-on))
;; https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :ensure t)

;; ;; https://github.com/nex3/perspective-el
;; (use-package perspective
;;   :ensure t
;;   :config
;;   (persp-mode))
;; (use-package persp-projectile
;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES

;; themes from the packages repository:
(use-package afternoon-theme    :ensure t :defer t)
(use-package ample-zen-theme    :ensure t :defer t)
(use-package blackboard-theme   :ensure t :defer t)
(use-package darkburn-theme     :ensure t :defer t)
(use-package darkmine-theme     :ensure t :defer t)
(use-package darktooth-theme    :ensure t :defer t)
(use-package eclipse-theme      :ensure t :defer t)
(use-package hc-zenburn-theme   :ensure t :defer t)
(use-package idea-darkula-theme :ensure t :defer t)
(use-package lush-theme         :ensure t :defer t)
(use-package material-theme     :ensure t :defer t)
(use-package naquadah-theme     :ensure t :defer t)
(use-package reverse-theme      :ensure t :defer t)
(use-package tangotango-theme   :ensure t :defer t)
(use-package zenburn-theme      :ensure t :defer t)

;; hand picked themes:
;; https://github.com/emacs-jp/replace-colorthemes
;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; choose one of them:
;; (load-theme 'charcoal-black t t)
;; (enable-theme 'charcoal-black)
(load-theme 'afternoon t t)
(enable-theme 'afternoon)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFUNs

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
  (let* ((list [82 166 249])
         (i (energos/inc-or-dec
             (frame-parameter (selected-frame) 'energos/width)
             (1- (length list)) dec))
         (width (aref list i)))
    (set-frame-parameter (selected-frame) 'energos/width i)
    (set-frame-width (selected-frame) width)
    (message (format "Frame width resized to %d characters" width))))

;; --- Send a key event to another application in same desktop ---
(defun send-key-to-window-class (class key &optional noactivate)
  "Send a KEY event to a application of class CLASS in same desktop.
If last argument NOACTIVATE (optional) is t, will send the key without
activating the window. Some applications will refuse to accept the key
if its window is not active."
  (if noactivate (setq noactivate "NOACTIVATE") (setq noactivate ""))
  (if (executable-find "xdotool")
      (message (shell-command-to-string (format "\
class=\"%s\"; key=\"%s\"; noact=\"%s\"; \
window=$(xdotool search --desktop $(xdotool get_desktop) --classname ^${class}$ | head -1); \
if \[\[ -n ${window} \]\]; then \
xdotool keyup ${key}; \
\[\[ -z ${noact} \]\] && actual=$(xdotool getwindowfocus); \
echo -n Sending key \\'${key}\\' to \\'${class}\\' window; \
\[\[ -z ${noact} \]\] && xdotool windowactivate --sync ${window} key ${key} \|\| \
xdotool key --window ${window} ${key}; \
\[\[ -z ${noact} \]\] && xdotool windowactivate ${actual}; \
else echo -n No \\'${class}\\' window found in current desktop; fi"
                                                class key noactivate)))
    (message "No 'xdotool' executable found")))

;; --- Recarregar o Browser ---
(defun browser-reload ()
  "Reload current desktop browser window."
  (interactive)
  (send-key-to-window-class "Navigator" "F5"))

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

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files '("~/org/notes.org" "~/org/agenda.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . nil)
   (dot . t)
   (calc . t)
   (shell . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (ruby . t)
   (C . t)
   (python . t)
   (scheme . t)
   ))

;; --- DANGER! ---
;; Avaliar código sem exigir confirmação
;; https://orgmode.org/manual/Code-evaluation-security.html
;; --- DANGER! ---
(setq org-confirm-elisp-link-function nil)
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (or
              ;; t                        ; não pergunto nada
              (string= lang "ditaa")      ; não pergunto por ditaa
              (string= lang "dot")        ; ...
              (string= lang "calc")
              (string= lang "emacs-lisp")
              (string= lang "elisp")
              (string= lang "C")
              (string= lang "C++")
              (string= lang "sh")         ; https://emacs.stackexchange.com/questions/35321/
              (string= lang "shell")      ;
              (string= lang "ruby")
              (string= lang "python")
              (string= lang "scheme")
              ))))

;; https://emacs.stackexchange.com/questions/2387/browser-not-opening-when-exporting-html-from-org-mode
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . "xdg-open %s")
        ("\\.djvu\\'" . "xdg-open %s")
        ("\\.pdf\\'" . "xdg-open %s")))

;; --- preferences ---
(setq org-replace-disputed-keys t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-link-frame-setup
      (quote
       ((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame))))
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(setq org-export-coding-system 'utf-8)

;; --- org-refile ---
;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-targets '((nil . (:maxlevel . 6)) (org-agenda-files . (:maxlevel . 6))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; --- org-capture ---
(require 'org-protocol)
(require 'org-capture)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %u %?" :prepend t)
        ("j" "Journal entry" entry (file+datetree "journal.org")
         "* %U %^{Title}\n  %?")
        ("n" "Note" entry (file+headline org-default-notes-file "Unsorted Notes")
         "* %?\n  %i\n  %a")
        ))
;; See org-capture-templates help:
;; %?          After completing the template, position cursor here.
;;
;; %i          Initial content, copied from the active region.  If %i is
;;             indented, the entire inserted text will be indented as well.
;;
;; %a          Annotation, normally the link created with `org-store-link'.
;; %t          Time stamp, date only.
;; %T          Time stamp, with date and time.
;; %u, %U      Like the above, but inactive time stamps.

;; --- link to man pages ---
;; https://www.gnu.org/software/emacs/manual/html_node/org/Adding-hyperlink-types.html
;; org-man.el is archived in package org-plus-contrib at https://orgmode.org/elpa.html
(require 'org-man)
(setq org-man-command 'man)

;; --- org-mode specific shortcuts ---
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD SHORTCUTS

;; --- We don't need a freaking mouse wheel ---
(mouse-wheel-mode -1)
(global-set-key (kbd "<mouse-4>") 'ignore)
(global-set-key (kbd "<mouse-5>") 'ignore)

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
(global-set-key (kbd "<f11>") 'energos/resize-frame)
(global-set-key (kbd "S-<f11>")
                (lambda () (interactive) (energos/resize-frame t)))

(global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

;; --- Symbol highlighting ---
(global-set-key (kbd "H-H") 'highlight-symbol-all-windows)
(global-set-key (kbd "H-h") 'highlight-symbol)

(global-set-key (kbd "H-U") 'unhighlight-symbol-all-windows)
(global-set-key (kbd "H-u") 'unhighlight-symbol)

(global-set-key (kbd "H-A") 'unhighlight-all-windows)
(global-set-key (kbd "H-a") 'unhighlight)

;; --- Frequently used files ---
(global-set-key (kbd "\e\ei") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "\e\en") (lambda () (interactive) (find-file org-default-notes-file)))
(global-set-key (kbd "\e\es") (lambda () (interactive) (switch-to-buffer "*scratch*")))

;; --- Insert key ---
;; I keep accidentally hitting that damn key
(global-set-key (kbd "<insert>") (lambda () (interactive) (overwrite-mode -1)))
(global-set-key (kbd "C-x <insert>") 'overwrite-mode)

;; --- Kill buffers ---
;; Too much noise in "helm-mode-kill-buffer"
(global-set-key (kbd "C-x K") 'kill-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;; Never kill *scratch* or *Messages*
(with-current-buffer "*scratch*"
	  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
	  (emacs-lock-mode 'kill))

;; --- CapsLock -> F13 -> Hyper ---
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-K") 'kill-buffer)
(global-set-key (kbd "H-k") 'kill-current-buffer)
(global-set-key (kbd "H-b") 'helm-buffers-list)
(global-set-key (kbd "H-f") 'helm-find-files)
(global-set-key (kbd "H-c") (kbd "C-c C-c"))
(global-set-key (kbd "H-e") (kbd "C-x C-e"))
(global-set-key (kbd "H-<f13>") (kbd "C-x C-e"))
(global-set-key (kbd "H-d") 'geiser-doc-symbol-at-point)
(global-set-key (kbd "H-m") 'magit-status)
(global-set-key (kbd "H-Q") 'save-buffers-kill-terminal)
(global-set-key (kbd "H-n") 'make-frame-command)

(global-set-key (kbd "H-g")
                (lambda () "Set C-x C-e to call geiser-eval-last-sexp."
                  (interactive)
                  (progn (local-set-key (kbd "C-x C-e") 'geiser-eval-last-sexp)
                         (message "C-x C-e will call geiser-eval-last-sexp"))))
(global-set-key (kbd "H-G")
                (lambda () "Set C-x C-e to call the default command."
                  (interactive)
                  (progn (local-unset-key (kbd "C-x C-e"))
                         (message "C-x C-e reset to default"))))

(global-set-key (kbd "H-0")
                (lambda () "Delete window or delete frame if there is only one window."
                  (interactive) (if (one-window-p) (delete-frame) (delete-window))))
(global-set-key (kbd "H-1") 'delete-other-windows)
(global-set-key (kbd "H-2") 'split-window-below)
(global-set-key (kbd "H-3") 'split-window-horizontally)

;; --- Send a key event to another application        ---
;; --- WARNING! Send the same key used as a shortcut! ---
(global-set-key (kbd "H-<f5>") 'browser-reload)
(global-set-key (kbd "H-<prior>")
                (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "Prior")))
(global-set-key (kbd "H-<next>")
                (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "Next")))
(global-set-key (kbd "H-<home>")
                (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "Home")))
(global-set-key (kbd "H-<end>")
                (lambda () (interactive) (send-key-to-window-class "org.pwmt.zathura" "End")))

;; --- Make ---
(global-set-key (kbd "<f9>") (lambda () "Make all" (interactive) (save-buffer 0) (compile "make")))
(global-set-key (kbd "S-<f9>") (lambda () "Make clean" (interactive) (save-buffer 0) (compile "make clean")))
(global-set-key (kbd "C-<f9>") (lambda () "Make flash" (interactive) (save-buffer 0) (compile "make flash")))
(global-set-key (kbd "C-S-<f9>") (lambda () "Make verify" (interactive) (compile "make verify")))
(global-set-key (kbd "M-<f9>") (lambda () "Make reset" (interactive) (compile "make reset")))

(setq compilation-window-height 24)
(setq compilation-scroll-output 'first-error)

;; --- Support for keyboard physical macro keys ---
;;
;; M1 - ESC <f1> -> C-c &
;; M2 - ESC <f2> -> C-c C-o
;; M3 - ESC <f3> -> C-c C-c
;; M4 - ESC <f4> -> C-x C-e
;; M5 - ESC <f5> -> C-x

;; m1 map
(define-prefix-command 'energos/m1-map)
(define-key energos/m1-map (kbd "<f1>") (kbd "C-x C-e"))
(define-key energos/m1-map (kbd "<f2>") (kbd "C-x C-e"))
(define-key energos/m1-map (kbd "<f3>") (kbd "C-x C-e"))
(define-key (current-global-map) (kbd "H-<escape>") energos/m1-map)

;; M1
(define-key key-translation-map (kbd "ESC <f1>") (kbd "C-c &"))

;; M2
(define-key key-translation-map (kbd "ESC <f2>") (kbd "C-c C-o"))

;; M3
(define-key key-translation-map (kbd "ESC <f3>") (kbd "C-c C-c"))

;; M4
(define-key key-translation-map (kbd "ESC <f4>") (kbd "C-x C-e"))

;; M5
(define-key key-translation-map (kbd "ESC <f5>") (kbd "C-x"))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AREA 51
(load (expand-file-name "experimental.el" user-emacs-directory))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START SERVER!
(require 'server)
(unless (server-running-p)
  (server-start))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZE
;; Keep this file free from customize data
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
