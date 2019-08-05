(let* ((current-directory (file-name-directory load-file-name))
       (root-path (expand-file-name ".." current-directory))
       (test-path (expand-file-name "." current-directory))
       (test-file (expand-file-name "evil-owl-test.el" test-path)))

  (add-to-list 'load-path root-path)
  (add-to-list 'load-path test-path)

  (setq load-prefer-newer t)
  (load test-file nil t)

  (ert-run-tests-batch-and-exit))
