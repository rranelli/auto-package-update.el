;;; auto-package-update.el --- Automatically update Emacs packages.

;; Copyright (C) 2014 Renan Ranelli <renanranelli at google mail>

;; Author: Renan Ranelli
;; URL: http://github.com/rranelli/auto-package-update.el
;; Version: 0.1
;; Keywords: package, update
;; Package-Requires: ((emacs "24.4"))

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:

;; This script provides functionality for automatically updating your Emacs
;; packages periodically. It is specially usefull for people that work in
;; multiple machines and tend to forget to manually update packages from time to
;; time. The periodicity of the update is given by the custom variable
;; `auto-package-update-interval'.
;;
;;; Change Log:
;;
;; 0.1 - First release

;;; Code:
(require 'package)


;;
;;; Customization
;;
(defcustom auto-package-update-interval
  7
  "Interval in DAYS for automatic package update."
  :group 'init-packages
  :type 'int)

(defvar apu--last-update-day-filename
  ".last-package-update-day"
  "Name of the file in which the last update day is going to be stored.")

(defvar apu--last-update-day-path
  (expand-file-name apu--last-update-day-filename user-emacs-directory)
  "Path to the file that will hold the day in which the last update was run.")

;;
;;; File read/write helpers
;;
(defun apu--read-file-as-string (file)
  "Read FILE contents."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun apu--write-string-to-file (file string)
  "Substitute FILE contents with STRING."
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
		    (point-max)
		    file))))

;;
;;; Update day read/write functions
;;
(defun apu--today-day ()
  (time-to-days (current-time)))

(defun apu--write-current-day ()
  "Store current day."
  (apu--write-string-to-file
   apu--last-update-day-path
   (int-to-string (apu--today-day))))

(defun apu--read-last-update-day ()
  "Read last update day."
  (string-to-number
   (apu--read-file-as-string apu--last-update-day-path)))

;;
;;; Package update
;;
(defun apu--should-update-packages-p ()
  (or
   (not (file-exists-p apu--last-update-day-path))
   (let* ((last-update-day (apu--read-last-update-day))
	  (days-since (- (apu--today-day) last-update-day)))
     (>=
      (/ days-since auto-package-update-interval)
      1))))

;;;###autoload
(defun auto-package-update-now ()
  "Update installed Emacs packages."
  (interactive)
  (save-excursion
    (package-refresh-contents)
    (package-list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute t)
    (kill-buffer))
  (apu--write-current-day)
  (message "[PACKAGES UPDATED]"))

;;;###autoload
(defun auto-package-update-maybe ()
  "Update installed Emacs packages if needed."
  (when (apu--should-update-packages-p)
    (auto-package-update-now)))

(provide 'auto-package-update)
;;; auto-package-update.el ends here
