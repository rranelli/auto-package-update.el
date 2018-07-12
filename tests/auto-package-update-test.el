(require 'auto-package-update)

(ert-deftest test-should-update-if-there-is-no-record-file ()
  (setq auto-package-update-last-update-day-path "/I/be/no/file")
  (should (apu--should-update-packages-p)))

(ert-deftest test-dash-filter-usage ()
  (should (equal
	   '(1 2 3)
	   (-filter #'(lambda (x) (< x 4)) '(1 2 3 4 5)))))

(ert-deftest test-safe-install-wont-raise-error ()
  (apu--safe-install-packages '(idonotexistasapackageyay)))

(ert-deftest test-it-works ()
  (defun apu--packages-to-install ()
    '(2048-game))
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (auto-package-update-now))

(ert-deftest test-should-update ()
  (defun apu--read-last-update-day ()
    (- (apu--today-day) auto-package-update-interval 2))
  (should (apu--should-update-packages-p)))

(ert-deftest test-should-not-update-if-today-it-was-updated ()
  (defun apu--read-last-update-day ()
    (apu--today-day))
  (defun file-exists-p (f)
    t)
  (should (not (apu--should-update-packages-p))))

(provide 'auto-package-update-test)
;;; auto-package-update-test.el ends here
