(require 'auto-package-update)

(apu-write-current-day)

(ert-deftest test-should-not-update-if-today-it-was-updated ()
  (defun apu-read-last-update-day ()
    (apu-today-day))
  (should (not (apu-should-update-packages-p))))

(ert-deftest test-should-update-if-there-is-no-record-file ()
  (setq apu-last-update-day-path "/I/be/no/file")
  (should (apu-should-update-packages-p)))

(ert-deftest test-should-update ()
  (defun apu-read-last-update-day ()
    (- (apu-today-day) apu-package-update-interval 2))
  (should (apu-should-update-packages-p)))

(provide 'auto-package-update-test)
;;; auto-package-update-test.el ends here
