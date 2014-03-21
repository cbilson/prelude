(eval-when-compile (require 'psake))

;; TODO: csharp-test-mode?
(defun webui--find-fixture-name ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward "[TestFixture]")
    (next-line)
    (beginning-of-line)
    (search-forward "class")
    (forward-char)
    (let ((beg (point)))
      (forward-word)
      (thing-at-point 'word))))

(defun webui-compile-js ()
  "Run our javascript/coffeescript bundling"
  (interactive)
  (psake-execute "Compile-Javascript"))

(defun webui-unit-test ()
  "Run all our unit tests"
  (interactive)
  (psake-execute "Unit-Test"))

(defun webui-integration-test ()
  "Run all our integration tests"
  (interactive)
  (psake-execute "Integration-Test"))

(defun webui-web-test ()
  "Run all the web tests"
  (interactive)
  (psake-execute "Web-Integration-Test"))

(defun webui-web-test-current-fixture ()
  "Run the current web integration test fixture"
  (interactive)
  (let ((fixture-name (webui--find-fixture-name)))
    (message fixture-name)
    (psake-execute "Web-Integration-Test"
                         '(("webtest_testFixture" . fixture-name)))))

(defun webui-compile ()
  "Compile the application into the build directory"
  (interactive)
  (psake-execute "Compile"))

(defun webui-iis ()
  "Run the app from the build directory with IIS Express"
  (interactive)
  (psake-execute "Start-IIS"))

(defun webui-browse ()
  "Open a browser to the index page"
  (interactive)
  (browse-url "http://localhost:12345/"))

(defun webui-csharp-mode-hook ()
  (local-set-key (kbd "C-c ,") 'webui-unit-test))

(defun webui-create-users ()
  (interactive)
  (psake-execute "Create-Users"))

(add-hook 'csharp-mode-hook 'webui-csharp-mode-hook)

(provide 'webui)
