;; Copyright (c) 2013, 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
;;

(when (and (package-installed-p 'mmm-mode)
           (package-installed-p 'lua-mode))
  (require 'mmm-mode)
  (require 'mmm-auto)
  (require 'lua-mode)
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 0)
  (eval-after-load 'mmm-vars
    '(progn
       (mmm-add-group
        'lean-lua
        '((lua-inline
           :submode lua-mode
           :face mmm-code-submode-face
           :front "[(][*]"
           :back "[*][)]")))
       (mmm-add-mode-ext-class 'lean-mode "\\.lean" 'lean-lua))))

(provide 'lean-mmm-lua)
