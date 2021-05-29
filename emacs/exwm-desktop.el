;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO!
;; transparency / wallpaper
;; F11 / resize frame
;; printscreen / multimedia keys
;; remove ~/bin/pqp dependency

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exwm

;; Start some stuff
(start-process-shell-command "feh" nil "~/.fehbg")
(start-process-shell-command "volumeicon" nil "volumeicon")
(start-process-shell-command "compton" nil "compton")
(start-process-shell-command "parcellite" nil "parcellite")

;; Define some stuff
;; using ~/bin/pqp as a stopgap
(defun energos/volume-up () (interactive)
  (start-process-shell-command "volume-up" nil "pqp volume up"))
(defun energos/volume-down () (interactive)
  (start-process-shell-command "volume-down" nil "pqp volume down"))
(defun energos/volume-toggle () (interactive)
  (start-process-shell-command "volume-toggle" nil "pqp volume toggle"))
(defun energos/screen-lock () (interactive)
  (start-process-shell-command "screen-lock" nil "slock"))
(defun energos/print-screen () (interactive)
  (start-process-shell-command "print-screen" nil "spectacle"))
(defun energos/music-player () (interactive)
  (start-process-shell-command "music-player" nil "pqp cmus"))
(defun energos/terminal () (interactive)
  (start-process-shell-command "terminal" nil "pqp"))
(defun energos/new-terminal () (interactive)
  (start-process-shell-command "new-terminal" nil "pqp -n"))
(defun energos/navigator () (interactive)
  (start-process-shell-command "navigator" nil "firefox"))
(defun energos/new-navigator () (interactive)
  (start-process-shell-command "new-navigator" nil "firefox"))

;; The real deal
(use-package exwm
  :ensure t
  :config

  ;; copied and fubared from exwm-config.el

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 6)
  ;; Start at workspace 1
  (add-hook 'exwm-init-hook
            (lambda ()
              (exwm-workspace-switch-create 1)))

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Global keybindings.
  (setq exwm-input-global-keys
        `(
          ([f14]     . energos/volume-down)
          ([f15]     . energos/volume-up)
          ([print]   . energos/print-screen)
          ([S-f14]   . energos/volume-toggle)
          ([S-f15]   . energos/music-player)
          ([S-print] . (lambda () (interactive) (message "You pressed the very ugly Shift+PrintScreen key")))
          ([?\s-l]   . energos/screen-lock)
          ([?\s-t]   . energos/terminal)
          ([?\s-T]   . energos/new-terminal)
          ([?\s-i]   . energos/navigator)
          ([?\s-I]   . energos/new-navigator)

          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left]  . windmove-left)
          ([s-right] . windmove-right)
          ([s-up]    . windmove-up)
          ([s-down]  . windmove-down)

          ;; 's-w': Switch workspace.
          ([?\s-w]   . exwm-workspace-switch)
          ;; 's-&': Launch application.
          ([?\s-&]   . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
          ;; 's-N': Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Enable EXWM
  (exwm-enable)

  )
