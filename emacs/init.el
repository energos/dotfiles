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
(line-number-mode 1)                    ; mostrar linha no 'mode line'
(column-number-mode 1)                  ; mostra coluna no 'mode-line'
(show-paren-mode 1)                     ; destacar par de parenteses
(setq frame-resize-pixelwise t)         ; pixel perfect resize
(setq-default truncate-lines t)         ; disable line wrap
(setq truncate-partial-width-windows nil)
(winner-mode 1)

(setq scroll-step 1)                    ; scroll de apenas 1 linha
(setq scroll-preserve-screen-position t); PgUp/PgDown mantém posição do cursor,
(setq scroll-error-top-bottom t)        ; exceto na primeira e última página

(defalias 'yes-or-no-p 'y-or-n-p)       ; "y or n" em vez de "yes or no"
(setq confirm-nonexistent-file-or-buffer t) ; confirmar criação arquivo/buffer

;; ediff sem frame de controle e com janelas lado a lado
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defalias 'list-buffers 'ibuffer)

;; --- Não expiro senhas - DANGER! ---
(setq password-cache-expiry nil)

;; --- Freaking TAB behaviour
(setq-default tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(electric-indent-mode -1)

;; --- Ido mode ---
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (setq ido-use-filename-at-point 'guess)
;; (ido-mode 1)
;;
;; ido vs. helm vs. counsel ?
;; let's disable ido-mode and give counsel, or helm, a ride...

;; --- Mensagem inicial ---
(setq initial-scratch-message
      (let ((string "")
            (list (split-string (shell-command-to-string "fortune") "\n")))
        (dolist (line list)
          (setq string (concat string
                               ";;"
                               (if (= 0 (length line)) "" "  ")
                               line
                               "\n")))
        string))

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

(use-package framemove
  :ensure t
  :init
  (require 'cl)
  :config
  (setq framemove-hook-into-windmove t))

;; https://www.emacswiki.org/emacs/HelpPlus
(use-package help-fns+
  :ensure t
  :config
  (require 'help-fns+))

(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq vc-handled-backends nil))

;; https://github.com/emacs-helm/helm
(use-package helm
  :ensure t
  :bind (("C-x b"   . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring))
  :config
  (require 'helm-config)
  (setq helm-split-window-default-side 'other)
  (helm-mode 1))

;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :ensure t
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  (require 'helm)
  (require 'helm-swoop))

;; https://github.com/areina/helm-dash
(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-docsets-path (concat (getenv "HOME") "/.emacs.d/docsets"))
  (defun energos/dash-install (docset)
    (if (helm-dash-docset-installed-p docset)
        (message (format "%s docset is already installed!" docset))
      (progn (message (format "Installing %s docset..." docset))
             ; Arghh, there is a freaking mess between " " and "_"
             (helm-dash-install-docset (subst-char-in-string ?\s ?_ docset)))))
  (energos/dash-install "Apache_HTTP_Server")
  (energos/dash-install "C")
  (energos/dash-install "Bash")
  (energos/dash-install "Emacs Lisp")
  (energos/dash-install "Common Lisp")
  (energos/dash-install "HTML")
  (energos/dash-install "CSS")
  (energos/dash-install "Rust")
  (energos/dash-install "Go")
  (energos/dash-install "Haskell")

  (setq helm-dash-browser-func 'eww)
  ;; (setq helm-dash-browser-func 'browse-url)

  (defun energos/dash-elisp ()
    (setq-local helm-dash-docsets '("Emacs Lisp")))
  (add-hook 'emacs-lisp-mode-hook 'energos/dash-elisp)

  (defun energos/dash-bash ()
    (setq-local helm-dash-docsets '("Bash")))
  (add-hook 'sh-mode-hook 'energos/dash-bash)
)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES

;; themes from the packages repository:
(use-package afternoon-theme    :ensure t)
;; (use-package ample-zen-theme    :ensure t)
;; (use-package blackboard-theme   :ensure t)
;; (use-package darkburn-theme     :ensure t)
;; (use-package darkmine-theme     :ensure t)
;; (use-package eclipse-theme      :ensure t)
;; (use-package hc-zenburn-theme   :ensure t)
;; (use-package idea-darkula-theme :ensure t)
;; (use-package lush-theme         :ensure t)
;; (use-package material-theme     :ensure t)
;; (use-package naquadah-theme     :ensure t)
;; (use-package reverse-theme      :ensure t)
;; (use-package tangotango-theme   :ensure t)
;; (use-package zenburn-theme      :ensure t)

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

;; https://www.emacswiki.org/emacs/SearchAtPoint#toc7
(defun backward-symbol (arg)
  "Move point to the previous position that is the beggining of a symbol."
  (interactive "p")
  (forward-symbol (- arg)))

;; https://www.emacswiki.org/emacs/NavigatingParentheses
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis.
Else go to the opening parenthesis one level up."
  (interactive "p")
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
  "Return 1 if N is not an integer greater or equal to zero.
If DEC is t, return N-1 if 0<N≤MAX, return 0 if N=0, return MAX if MAX<N.
If DEC is nil or absent, return N+1 if N<MAX, return MAX if MAX≤N."
  (or
   (and (or (not (integerp n)) (< n 0)) 1)
   (and dec
        (or
         (and (> n max) max)
         (and (> n 0) (1- n)) 0))
   (and (>= n max) max)
   (1+ n)
   )
  )

(defun energos/resize-frame (&optional dec)
  "If DEC is t, decrease current frame size, else increase current frame size."
  (interactive "P")
  (let* (
         (list [88 178 267])
         (i (energos/inc-or-dec
             (get 'energos/frame-width 'index) (1- (length list)) dec))
         (width (aref list i))
         )
    (put 'energos/frame-width 'index i)
    (set-frame-width (selected-frame) width)
    (message (format "Frame width resized to %d characteres" width))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD SHORTCUTS
(global-set-key (kbd "<f13>") 'ignore)
(define-key key-translation-map (kbd "<f13>") 'event-apply-hyper-modifier)

(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-<up>") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (scroll-up 1)))

(global-set-key (kbd "M-<up>")
                (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "M-<down>")
                (lambda () (interactive) (scroll-other-window 1)))

(global-set-key (kbd "H-<left>") 'windmove-left)
(global-set-key (kbd "H-<right>") 'windmove-right)
(global-set-key (kbd "H-<up>") 'windmove-up)
(global-set-key (kbd "H-<down>") 'windmove-down)

(global-set-key (kbd "<f12>") 'toggle-truncate-lines)

(global-set-key (kbd "C-<prior>")
                (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-<next>")
                (lambda () (interactive) (move-to-window-line -1)))

(global-set-key (kbd "C-<tab>")
                (lambda (arg) "Insert TAB." (interactive "P") (insert-tab arg)))

(global-set-key (kbd "M-b") 'backward-symbol)
(global-set-key (kbd "M-f") 'forward-symbol)

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START SERVER!
(require 'server)
(unless (server-running-p)
  (server-start))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WTF!!!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (afternoon-theme helm-dash helm-swoop helm magit help-fns+ framemove which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
