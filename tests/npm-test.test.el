(defmacro with-fixture-directory (relative-path body)
  `(unwind-protect
      (let ((default-directory (expand-file-name ,relative-path user-emacs-directory)))
        ,body)))

(ert-deftest test-npm-test--test-scripts-list ()
  (with-fixture-directory "tests/fixtures"
   (should (equal (npm-test--test-scripts-list)
                  '("npm run test"
                    "npm run test:cli"
                    "npm run test:api"
                    "npm run test:watch")))))

(ert-deftest test-npm-test--node-project-root ()
  (with-fixture-directory "tests/fixtures/some-folder/"
    (should (equal (expand-file-name (npm-test--node-project-root))
                   (expand-file-name "tests/fixtures/" user-emacs-directory)))))
