;;; control-mode.el --- a global minor mode for Emacs that gives a "control" mode, similar in purpose to vim's "normal" mode

;; Copyright (C) 2013 Stephen Marsh

;; Version: 0.1

;; Author: Stephen Marsh <stephen.david.marsh@gmail.com>

;; Keywords: convenience emulations
;; Url: https://github.com/stephendavidmarsh/control-mode

;; Control mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Control mode is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Control mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Control mode is a global minor mode for Emacs that provides a
;; "control" mode, similar in purpose to vim's "normal" mode. Unlike
;; the various vim emulation modes, the key bindings in Control mode
;; are derived from the key bindings already setup, usually by making
;; the control key unnecessary, e.g. "Ctrl-f" becomes "f". This
;; provides the power of a mode dedicated to controlling the editor
;; without needing to learn or maintain new key bindings. See
;; https://github.com/stephendavidmarsh/control-mode for complete
;; documentation.

;;; Code:

; for flet
(eval-when-compile (require 'cl))

(defvar control-mode-overrideable-bindings '(nil self-insert-command undefined))

; Ignore Control-M
(defvar control-mode-ignore-events '(13))

; This can't be control-mode-map because otherwise the
; define-minor-mode will try to install it as the minor mode keymap,
; despite being told the minor mode doesn't have a keymap.
(defvar control-mode-keymap (make-sparse-keymap))

(defvar control-mode-keymap-generation-functions '())

(defvar control-mode-conversion-cache '())

; This will be buffer local
(defvar control-mode-emulation-alist nil)

(defun control-mode-create-alist ()
  (setq control-mode-emulation-alist
        (let* ((mode-key (cons major-mode (sort (mapcar (lambda (x) (car (rassq x minor-mode-map-alist))) (current-minor-mode-maps)) 'string<)))
               (value (assoc mode-key control-mode-conversion-cache)))
          (if value (cdr value)
            (let ((newvalue (mapcar (lambda (x) (cons t x)) (cons (control-mode-create-hook-keymap) (cons control-mode-keymap (mapcar 'get-converted-keymap-for (current-active-maps)))))))
              (push (cons mode-key newvalue) control-mode-conversion-cache)
              newvalue)))))

(defun control-mode-create-hook-keymap ()
  (let ((keymap (make-sparse-keymap)))
    (run-hook-with-args 'control-mode-keymap-generation-functions keymap)
    keymap))

(defun control-mode-mod-modifiers (event f)
  (event-convert-list (append (funcall f (remq 'click (event-modifiers event))) (list (event-basic-type event)))))

(defun get-converted-keymap-for (keymap)
  (let ((auto-keymap (make-sparse-keymap)))
    ; Namespaces? What's that?
    (flet ((add-binding (e b) (define-key auto-keymap (vector e) b))
           (key-bindingv (e) (key-binding (vector e)))
           (remove-modifier (event mod) (control-mode-mod-modifiers event (lambda (x) (remq mod x))))
           (add-modifier (event mod) (control-mode-mod-modifiers event (lambda (x) (cons mod x))))
           (add-modifiers (event mod1 mod2) (control-mode-mod-modifiers event (lambda (x) (cons mod2 (cons mod1 x)))))
           (is-overrideable (b) (memq (key-bindingv b) control-mode-overrideable-bindings))

           (try-to-rebind (e b)
                          (if (not (is-overrideable e)) nil
                            (add-binding e b)
                            t)))
          (map-keymap 'control-mode-handle-binding keymap)
          auto-keymap)))

(defun control-mode-handle-binding (event binding)
  (unless (memq event control-mode-ignore-events)
    (if (memq 'control (event-modifiers event))
        (let ((newevent (remove-modifier event 'control)))
          (when (try-to-rebind newevent binding)
            (unless (memq 'meta (event-modifiers event)) ; Here to be safe, but Meta events should be inside Escape keymap
              (let ((cmbinding (key-bindingv (add-modifier event 'meta))))
                (when cmbinding
                  (add-binding event cmbinding)
                  (let ((metaevent (add-modifier newevent 'meta)))
                    (try-to-rebind metaevent cmbinding)))))))))
  (if (and (eq event 27)
           (keymapp binding))
      (map-keymap 'control-mode-handle-escape-binding binding)))

(defun control-mode-handle-escape-binding (event binding)
  (unless (memq event control-mode-ignore-events)
    (if (memq 'control (event-modifiers event))
        (let ((only-meta (add-modifier (remove-modifier event 'control) 'meta))
              (only-shift (add-modifier (remove-modifier event 'control) 'shift)))
          (try-to-rebind only-meta binding)
          (try-to-rebind event binding)
          (if (and control-mode-rebind-to-shift
                   (not (memq 'shift (event-modifiers event)))
                   (not (key-bindingv (add-modifier only-shift 'control)))
                   (not (key-bindingv (add-modifier only-shift 'meta))))
              (try-to-rebind only-shift binding)))
      (let ((control-instead (add-modifier event 'control)))
        (when (and (is-overrideable event)
                   (or (not (key-bindingv control-instead))
                       (memq control-instead control-mode-ignore-events)))
          (add-binding event binding)
          (let ((cmbinding (key-bindingv (add-modifiers event 'control 'meta))))
            (when cmbinding
              (add-binding (add-modifier event 'meta) cmbinding)
              (add-binding control-instead cmbinding))))))))

(define-minor-mode control-mode
  "Toggle Control mode.
With a prefix argument ARG, enable Control mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Control mode is a global minor mode."
  nil " Control" nil :global t (if control-mode (control-mode-setup) (control-mode-teardown)))

(defun control-mode-setup ()
  (in-all-buffers 'control-mode-buffer-setup)
  (add-hook 'emulation-mode-map-alists 'control-mode-emulation-alist)
  (add-hook 'after-change-major-mode-hook 'control-mode-major-mode-change))

(defun control-mode-teardown ()
  (in-all-buffers 'control-mode-buffer-teardown)
  (remove-hook 'emulation-mode-map-alists 'control-mode-emulation-alist)
  (remove-hook 'after-change-major-mode-hook 'control-mode-major-mode-change))

(defun in-all-buffers (f)
  (mapc (lambda (buf)
          (unless (string-prefix-p " *Minibuf" (buffer-name buf))
            (with-current-buffer buf
              (funcall f))))
          (buffer-list)))

(defun control-mode-buffer-setup ()
  (setq-local control-mode-emulation-alist nil)
  (control-mode-create-alist))

(defun control-mode-buffer-teardown ()
  (kill-local-variable 'control-mode-emulation-alist))

(defun control-mode-major-mode-change ()
  (unless (string-prefix-p " *Minibuf" (buffer-name))
    (control-mode-buffer-setup)))

(defun control-mode-default-setup ()
  (define-key control-mode-keymap (kbd "C-z") 'control-mode)
  (global-set-key (kbd "C-z") 'control-mode)
  (add-hook 'control-mode-keymap-generation-functions
            (lambda (keymap)
              (if (eq (key-binding (kbd "C-x f")) 'set-fill-column)
                  (define-key keymap (kbd "x f") (lookup-key (current-global-map) (kbd "C-x C-f"))))
              (unless (key-binding (kbd "C-x x"))
                (define-key keymap (kbd "x x") (lookup-key (current-global-map) (kbd "C-x C-x")))))))

(defun control-mode-reload-bindings ()
  "Force Control mode to reload all generated keybindings."
  (interactive)
  (setq control-mode-conversion-cache '())
  (if control-mode
      (in-all-buffers 'control-mode-buffer-setup)))

(defcustom control-mode-rebind-to-shift nil
  "Allow rebinding Ctrl-Alt- to Shift-"
  :group 'control
  :type '(boolean)
  :set (lambda (x v)
         (setq control-mode-rebind-to-shift v)
         (control-mode-reload-bindings)))

(provide 'control-mode)

;;; control-mode.el ends here
