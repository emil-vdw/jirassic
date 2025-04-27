(require 'f)

(defconst jirassic-test-location
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the tests and fixtures.")

(defconst jirassic-fixture-location
  (f-join jirassic-test-location "fixtures")
  "Directory of the tests and fixtures.")

(provide 'jirassic-conftest)
