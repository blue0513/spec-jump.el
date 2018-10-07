;;; spec-jump --- Jump to the correspond spec file.

;; Copyright (C) 2018- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; URL: https://github.com/blue0513
;; Version: 0.0.1
;; Package-Requires: ((counsel "0.10.0"))

;;; Commentary:

;; Load this script
;;
;;   (require 'spec-jump)
;;
;; Under the Git control,
;; you can execute command go to the correspond spec file.
;;
;; M-x spec-jump RET

;;; Code:

(require 'counsel)

(defun spec-jump()
  (interactive)
  (let* ((orig-filename (file-name-nondirectory buffer-file-name))
	 (class-name (file-name-base buffer-file-name))
	 (spec-filename (concat class-name "_spec.rb")))
    (if (string-match "_spec.rb" orig-filename)
	(message "This is spec file.")
      (counsel-git spec-filename))))

;; * provide

(provide 'spec-jump)

;;; spec-jump.el ends here
