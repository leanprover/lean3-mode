;; -*- lexical-binding: t; -*-
;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Authors: Soonho Kong, Sebastian Ullrich
;;

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'lean-info)
(require 'lean-util)
(require 'lean-server)
(require 'lean-debug)
(require 'flymake)

(defun lean-fill-placeholder-cont (info-record)
  "Continuation for lean-fill-placeholder"
  (let ((synth (and info-record (plist-get info-record :synth))))
    (when synth
      (let ((synth-str
             (replace-regexp-in-string "?M_[0-9]+" "_" synth)))
        (when (cl-search " " synth-str)
          (setq synth-str (concat "(" synth-str ")")))
        (when (looking-at "_")
          (delete-char 1)
          (insert synth-str))))))

(defun lean-fill-placeholder ()
  "Fill the placeholder with a synthesized expression by Lean."
  (interactive)
  (lean-get-info-record-at-point 'lean-fill-placeholder-cont))

(cl-defun lean-info-record-to-string (info-record)
  "Given typeinfo, overload, and sym-name, compose information as a string."
  (cl-destructuring-bind (&key type tactic_params tactic_param_idx text doc full-id &allow-other-keys) info-record
    (let ((name-str (or full-id text))
          (type-str type)
          str)
      (when tactic_params
        (setq tactic_params (-map-indexed (lambda (i param)
                                            (if (eq i tactic_param_idx)
                                                (propertize param 'face 'eldoc-highlight-function-argument)
                                              param)) tactic_params))
        (setq type-str (mapconcat 'identity tactic_params " ")))

      (when (and name-str type-str)
        (setq str (format (if tactic_params "%s %s" "%s : %s")
                          (propertize name-str 'face 'font-lock-variable-name-face)
                          type-str)))
      (when doc
        (let ((doc (if (<= emacs-major-version 27)
                       (let ((lines (split-string doc "\n")))
                         (if (cdr lines)
                             (concat (car lines) " ⋯")
                           (car lines)))
                     doc)))
          (setq str (concat str
                            (format "\n%s"
                                    (propertize doc 'face 'font-lock-comment-face))))))
      str)))

(defvar-local lean-eldoc-documentation-cache nil)
(defvar-local lean-add-to-kill-ring nil)

(defun lean-eldoc-documentation-function-cont (info-record cb)
  "Continuation for lean-eldoc-documentation-function"
  (let ((info-string (and info-record (lean-info-record-to-string info-record))))
    (when info-string
      (when lean-add-to-kill-ring
        (setq lean-add-to-kill-ring nil)
        (kill-new
         (substring-no-properties info-string))))
    (setq lean-eldoc-documentation-cache (and info-string (format "%s" info-string)))
    (funcall cb lean-eldoc-documentation-cache)))

(defun lean-eldoc-documentation-function (&optional cb)
  "Show information of lean expression at point if any
Takes as argument an optional callback function, which defaults to `eldoc-message`"
  (interactive)
  (when (not (eq lean-server-check-mode 'nothing)) ; TODO(gabriel): revisit once info no longer reparses the file
    (lean-get-info-record-at-point
     (lambda (info-record)
       (lean-eldoc-documentation-function-cont info-record (or cb 'eldoc-message))))
    'non-nil-non-string))

(defun lean-show-type ()
  "Show information of lean-expression at point if any."
  (interactive)
  (setq lean-add-to-kill-ring lean-show-type-add-to-kill-ring)
  (if (<= emacs-major-version 27)
      (lean-eldoc-documentation-function)
    (eldoc-print-current-symbol-info t)))

(defconst lean-show-goal-buffer-name "*Lean Goal*")

(setq lean-show-goal--handler-mask nil)

(defun lean-show-goal--handler ()
  (if lean-show-goal--handler-mask
      (setq lean-show-goal--handler-mask nil)
    (let ((deactivate-mark)) ; keep transient mark
      (when (and (not (eq lean-server-check-mode 'nothing))
					; TODO(gabriel): revisit ^^ once info no longer reparses the file
		 (lean-info-buffer-active lean-show-goal-buffer-name))
	(lean-get-info-record-at-point
	 (lambda (info-record)
	   (let ((state (plist-get info-record :state)))
	     (unless (or (s-blank? state) (s-blank? (s-trim state)))
	       (lean-with-info-output-to-buffer lean-show-goal-buffer-name (princ state))))))))))

(defun lean-toggle-show-goal ()
  "Show goal at the current point."
  (interactive)
  (lean-toggle-info-buffer lean-show-goal-buffer-name)
  (lean-show-goal--handler))

(provide 'lean-type)
