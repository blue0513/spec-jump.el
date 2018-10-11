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

;; Ref: https://stackoverflow.com/a/23960720/8888451
(defun git-dir-path ()
  "Get git root dir."
  (let* ((path buffer-file-name)
	 (root (file-truename (vc-git-root path)))
	 (filename (file-name-nondirectory path))
	 (filename-length (length filename)))
    (let ((chunk (file-relative-name path root)))
      (substring chunk 0 (- (length chunk) filename-length)))))

(defun spec-jump--spec-to-original(filepath)
  "Jump to original file detected by FILEPATH."
  (let* ((original-dirpath (replace-regexp-in-string "spec/" "app/" (git-dir-path)))
	 (class-name (file-name-nondirectory filepath))
	 (original-filename (replace-regexp-in-string "_spec" "" class-name))
	 (original-filepath (concat original-dirpath original-filename)))
    (counsel-git original-filepath)))

(defun spec-jump--original-to-spec(filepath)
  "Jump to spec file detected by FILEPATH."
  (let* ((class-name (file-name-base filepath))
	 (spec-dirpath (replace-regexp-in-string "app/" "spec/" (git-dir-path)))
	 (spec-filepath (concat spec-dirpath class-name "_spec.rb")))
    (counsel-git spec-filepath)))

(defun spec-jump--is-spec-file(filepath)
  "Check it is spec file by FILEPATH."
  (string-match "_spec.rb" filepath))

(defun spec-jump()
  "Jump from original to spec, spec to original."
  (interactive)
  (let* ((filepath (buffer-file-name)))
    (if (spec-jump--is-spec-file filepath)
	(spec-jump--spec-to-original filepath)
      (spec-jump--original-to-spec filepath))))

;; * provide

(provide 'spec-jump)

;;; spec-jump.el ends here
