;;; tmux.el --- A tool for managing tmux sessions. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient)

(defvar tmux-session-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-i"        'tmux/list-sessions) ; tab
    map)
  "Keymap for Tmux Mode.")

;;;###autoload
(define-derived-mode tmux-session-mode special-mode "Tmux Session Mode"
  "Mode for interacting with Tmux sessions."
  (buffer-disable-undo)
  (evil-make-overriding-map tmux-session-mode-map 'normal))


(defun tmux/list-sessions ()
  "List running sessions"
  (interactive)
  (message "sessions"))

(defun tmux/command ()
  (if (boundp 'ssh-addr)
      (tmux/--ssh-command)
    (message "else")))

(defun tmux/--ssh-command ()
  (message "todo"))

(let ((ssh-addr "blah.blah"))
  (tmux/command))
