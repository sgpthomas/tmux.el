;;; tmux.el --- A tool for managing tmux sessions. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient)


(define-derived-mode tmux-session-mode special-mode "Tmux Session Mode"
  "Mode for interacting with Tmux sessions."
  )


