;;; auto-package-update.el --- Automatically update Emacs packages.

;; Copyright (C) 2014 Renan Ranelli <renanranelli at google mail>

;; Author: Renan Ranelli
;; URL: http://github.com/rranelli/auto-package-update.el
;; Version: 0.1
;; Keywords: package, update
;; Package-Requires: ((emacs "24.3") (dash "2.1.0"))

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
;; 1.2 - Refactor for independency on package-menu functions.
;; 1.1 - Support GNU Emacs 24.3.
;; 1.0 - First release.

;;; Code:
(require 'dash)

(require 'package)
(package-initialize)


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

(defun apu--package-up-to-date-p (package)
  (when (package-installed-p package)
    (let* ((newest-desc (cadr (assq package package-archive-contents)))
	   (installed-desc (cadr (or (assq package package-alist)
				     (assq package package--builtins))))
	   (newest-version  (package-desc-version newest-desc))
	   (installed-version (package-desc-version installed-desc)))
      (version-list-<= newest-version installed-version))))

(defun apu--package-out-of-date-p (package)
  (not (apu--package-up-to-date-p package)))

(defun apu--packages-to-install ()
  (-filter 'apu--package-out-of-date-p package-activated-list))

(defun apu--safe-package-install (package)
  (condition-case ex
      (progn
	(package-install package)
	(add-to-list 'apu--package-installation-results
		     (format "%s up to date."
			     (symbol-name package))))
    ('error (add-to-list 'apu--package-installation-results
			 (format "Error installing %s"
				 (symbol-name package))))))

(defun apu--safe-install-packages (packages)
  (let (apu--package-installation-results)
    (dolist (package-to-update packages)
      (apu--safe-package-install package-to-update))
    apu--package-installation-results))

;;;###autoload
(defun auto-package-update-now ()
  "Update installed Emacs packages."
  (interactive)
  (package-refresh-contents)

  (let ((installation-report (apu--safe-install-packages (apu--packages-to-install))))
    (apu--write-current-day)
    (message (mapconcat #'identity
			(cons "[PACKAGES UPDATED]:" installation-report)
			"\n"))))

;;;###autoload
(defun auto-package-update-maybe ()
  "Update installed Emacs packages if needed."
  (when (apu--should-update-packages-p)
    (auto-package-update-now)))

(provide 'auto-package-update)
;;; auto-package-update.el ends here
