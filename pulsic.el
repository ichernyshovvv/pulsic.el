;;; pulsic.el --- Temporarily highlight the current line on window state change -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, pulse, highlight
;; URL: https://github.com/ichernyshovvv/pulsic

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The package provides a global minor mode that temporarily highlights the
;; current line on every window state change (see `window-state-change-hook'),
;; when `pulsic-predicate' is non-nil.

;; Similar package: pulsar.el

;;; Code:

(require 'pulse)
(require 'cus-edit)

(defgroup pulsic nil
  "Customization for `pulsic'."
  :group 'editing :prefix "pulsic-")

(defcustom pulsic-flag pulse-flag
  (custom-variable-documentation 'pulse-flag)
  :type (custom-variable-type 'pulse-flag))

(defcustom pulsic-delay pulse-delay
  (custom-variable-documentation 'pulse-delay)
  :type (custom-variable-type 'pulse-delay))

(defcustom pulsic-iterations pulse-iterations
  (custom-variable-documentation 'pulse-iterations)
  :type (custom-variable-type 'pulse-iterations))

;;;###autoload
(define-minor-mode pulsic-mode
  "Enable `pulsic-mode'."
  :global t
  (if pulsic-mode
      (add-hook 'window-state-change-hook #'pulsic-pulse)
    (remove-hook 'window-state-change-hook #'pulsic-pulse)))

(defface pulsic-line
  '((t :inherit highlight :extend t))
  "Default face for highlighting the current line in pulsic mode."
  :group 'pulsic)

(defcustom pulsic-predicate #'always
  "Predicate to call before running `pulsic-pulse'.
This only takes effect when `pulsic-mode' is enabled."
  :type 'function)

(defun pulsic-pulse ()
  "Pulse the current line, unhighlighting before next command."
  (when (funcall pulsic-predicate)
    (let* ((n (if (eobp) 0 1))
           (o (make-overlay (line-beginning-position n)
                            (1+ (line-end-position n))))
           (pulse-flag pulsic-flag)
           (pulse-delay pulsic-delay)
           (pulse-iterations pulsic-iterations))
      (overlay-put o 'pulse-delete t)
      (overlay-put o 'window (frame-selected-window))
      (pulse-momentary-highlight-overlay o 'pulsic-line))))

(provide 'pulsic)

;;; pulsic.el ends here
