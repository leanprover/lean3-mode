;;  -*- lexical-binding: t -*-
;;
;; Copyright (c) 2016 David Christiansen.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: David Christiansen
;;
;;; Code:

(require 'cl-lib)
(require 's)
(require 'lean-server)

(defface lean-message-boxes-content-face
  '((t :inherit font-lock-doc-face))
  "Face for Lean message box contents."
  :group 'lean)

(defcustom lean-message-boxes-enabled-captions '("check result" "eval result" "print result" "reduce result")
  "Which captions should result in boxes?"
  :group 'lean
  :type '(repeat (choice (const "check result")
                         (const "eval result")
                         (const "print result")
                         (const "reduce result")
                         (const "trace output"))))

(defcustom lean-message-boxes-enabledp nil
  "Whether or not to display message boxes."
  :group 'lean
  :type 'boolean)
(make-variable-buffer-local 'lean-message-boxes-enabledp)

(defun lean-message-boxes--ask-for-messages ()
  "Get the current messages out of the Lean server session."
  (let ((buf (current-buffer)))
    (if lean-server-session
        (cl-remove-if-not (lambda (msg)
                            (equal (buffer-file-name buf)
                                   (plist-get msg :file_name)))
                          (lean-server-session-messages lean-server-session))
      '())))

(defun lean-message-boxes--set-enabledp (enabledp)
  "Enable the boxes if ENABLEDP is non-nil."
  (setq lean-message-boxes-enabledp enabledp)
  (lean-message-boxes-display (lean-message-boxes--ask-for-messages)))

(defun lean-message-boxes-toggle ()
  "Toggle the display of message boxes."
  (interactive)
  (lean-message-boxes--set-enabledp (not lean-message-boxes-enabledp)))

(defun lean-message-boxes-enable ()
  "Enable the display of message boxes."
  (interactive)
  (lean-message-boxes--set-enabledp t))

(defun lean-message-boxes-disable ()
  "Disable the display of message boxes."
  (interactive)
  (lean-message-boxes--set-enabledp nil))

(defun lean-message-boxes--kill-overlays ()
  "Delete all Lean message overlays in the current buffer."
  (remove-overlays nil nil 'category 'lean-output))

(defun lean-message-boxes--pad-to (str width)
  "Pad the string STR to a particular WIDTH."
  (concat str (make-string (max 0 (- width (length str))) ?\ )))

(defun lean-message-boxes-display (msgs)
  "Show the messages MSGS in the Lean buffer as boxes when `lean-message-boxes-enabledp' is non-nil."
  (lean-message-boxes--kill-overlays)
  (when lean-message-boxes-enabledp
    (dolist (msg msgs)
      (let ((end-line (plist-get msg :end_pos_line))
            (end-col (plist-get msg :end_pos_col))
            (caption (plist-get msg :caption))
            (text (plist-get msg :text)))
        (when (member caption lean-message-boxes-enabled-captions)
          (lean-message-boxes--make-overlay
           end-line end-col
           caption text))))))

(defun lean-message-boxes--as-string (caption str)
  "Construct a propertized string representing CAPTION and STR."
  (let* ((str-copy (s-trim str)))
    (put-text-property 0 (length str-copy)
                       'face 'lean-message-boxes-content-face
                       str-copy)
    (let* ((lines (s-lines str-copy))
           (w (apply #'max (mapcar #'length (cons caption lines)))))
      (s-join "\n"
              (mapcar
               (lambda (l) (concat "│ " (lean-message-boxes--pad-to l w)))
               lines)))))

(defun lean-message-boxes--in-comment (pos)
  "Use the faces set by `font-lock-mode` to deduce whether the
character at the given position is contained within a comment."
  (let ((faces (get-text-property pos 'face))
        result)
    (unless (listp faces)
      (setq faces (list faces)))
    (dolist (f faces result)
      (setq result
            (or result (-contains? '(font-lock-comment-face font-lock-comment-delimiter-face) f))))))

(defun lean-message-boxes--make-overlay (end-line end-col caption text)
  "Construct a message box overlay at LINE and COL with CAPTION and TEXT."
  (let* ((end-pos (save-excursion (goto-char (point-min))
                                  (forward-line (1- end-line))
                                  (forward-char (1- end-col))
                                  (while (or (looking-at-p "[[:space:]\n]") (lean-message-boxes--in-comment (point)))
                                    (forward-char -1))
                                  (end-of-line)
                                  (point)))
         (overlay (make-overlay end-pos end-pos nil t t))
         (as-box (concat " \n" (lean-message-boxes--as-string caption text))))
    (put-text-property 0 (length as-box) 'cursor t as-box)
    (overlay-put overlay 'after-string as-box)
    (overlay-put overlay 'help-echo caption)
    (overlay-put overlay 'category 'lean-output)))

(add-hook 'lean-server-show-message-hook 'lean-message-boxes-display)
(provide 'lean-message-boxes)
