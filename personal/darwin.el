(when (eq system-type 'darwin)
  (progn

    (setq ls-lisp-use-insert-directory-program t
          insert-directory-program "gls")

    (set-frame-font "-outline-Consolas-normal-r-normal-normal-18-97-96-96-c-*-iso8859-1")

    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function 'paste-to-osx
          interprogram-paste-function 'copy-from-osx)

    (eval-after-load "locate"
      '(progn
         (setq locate-command "mdfind")))

    (defun stop-using-interprogram-cut-paste ()
      (interactive)
      (setq interprogram-cut-function nil)
      (setq interprogram-paste-function nil))

    (defun toggle-fullscreen ()
      "Toggle full screen"
      (interactive)
      (set-frame-parameter
       nil 'fullscreen
       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

    (global-set-key (kbd "C-c M-RET") 'toggle-fullscreen)

    (load (concat (file-name-directory load-file-name) "my-util"))

    (my-add-load-paths
     "/usr/local/share/emacs/site-lisp/mu4e"
     ;; "~/src/emacs-stuff/org-octopress"
     "~/src/emacs-stuff/google-translate")

    (prelude-require-packages '(google-translate csharp-mode grr))

    (setq browse-url-browser-function 'browse-url-default-macosx-browser
          user-mail-address "cbilson@pobox.com"
          mail-host-address "example.com")

    (defun my-grr-notify (message &rest args)
      "This is a little different from the grr version. Mainly I just wanted to set the icon."
      (let* ((start-process-args (my-grr-notify-build-args-list args))
             (process (apply #'start-process start-process-args)))
        (process-send-string process (grr-encode-string (grr-clean-string message)))
        (process-send-string process "\n")
        (process-send-eof process)))

    (defvar fail-image-filename (expand-file-name (concat user-emacs-directory "icons/orange-x.jpeg")))
    (defvar pass-image-filename (expand-file-name (concat user-emacs-directory "icons/green-ok.jpg")))

    (defun my-notify-fail (id msg)
      (my-grr-notify msg :id id :image fail-image-filename))

    (defun my-notify-pass (id msg)
      (my-grr-notify msg :id id :image pass-image-filename))

    (my-add-eval-after-init
     '(progn
        (require 'rbenv)
        (require 'mu4e)
        (require 'org-mu4e)
        (defalias 'mm 'mu4e)))

    (eval-after-load "mu4e"
      '(progn
         (defun mu4e-msgv-action-view-in-browser (msg)
           "View the body of the message in a web browser."
           (interactive)
           (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
                 (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
             (unless html (error "No html part for this message"))
             (with-temp-file tmpfile
               (insert
                "<html>"
                "<head><meta http-equiv=\"content-type\""
                "content=\"text/html;charset=UTF-8\">"
                html))
             (browse-url (concat "file://" tmpfile))))

         ;; see: https://github.com/djcb/mu/issues/284
         (defconst mu4e~view-url-regexp
           "\\(\\(https?\\://\\|mailto:\\)[-+\[:alnum:\].?_$%/+&#@!*~,:;=/]+\\)"
           "Regexp that matches http:/https:/mailto: URLs; match-string 1
will contain the matched URL, if any.")

         (defun new-mail-growl ()
           (my-grr-notify "New mail" :id "42"))

         (add-hook 'mu4e-index-updated-hook
                   'new-mail-growl)

         (add-to-list 'mu4e-view-actions
                      '("View in browser" . mu4e-msgv-action-view-in-browser) t)

         ;;(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

         (eval-after-load "org"
           '(setq org-mu4e-convert-to-html t))

         (setq mu4e-maildir       "~/Mail"      ;; top-level Maildir
               mu4e-sent-folder   "/sent"       ;; folder for sent messages
               mu4e-drafts-folder "/drafts"     ;; unfinished messages
               mu4e-trash-folder  "/trash"      ;; trashed messages
               mu4e-refile-folder "/archive"    ;; saved messages
               mu4e-get-mail-command "true"
               mu4e-headers-show-target nil
               mu4e-headers-skip-duplicates t
               mu4e-update-interval 60
               mu4e-headers-results-limit nil
               mu4e-sent-messages-behavior 'delete
               mu4e-html2text-command "w3m -dump -T text/html"
               mu4e-headers-toggle-skip-duplicates t
               mu4e-maildir-shortcuts
               '(("/GMail/[Gmail].Sent Mail"   . ?s)
                 ("/GMail/[Gmail].All Mail"    . ?a)
                 ("/Mantis/INBOX"              . ?m)
                 ("/drafts"                    . ?d)
                 ("/sent"                      . ?o)
                 ("/trash"                     . ?x))
               message-send-mail-function 'smtpmail-send-it
               message-kill-buffer-on-exit t
               smtpmail-default-smtp-server "smtp.gmail.com"
               smtpmail-smtp-server "smtp.gmail.com"
               smtpmail-stream-type 'starttls
               smtpmail-smtp-server "smtp.gmail.com"
               smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
               smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
               smtpmail-queue-mail t
               smtpmail-queue-dir (expand-file-name "~/Mail/queue")
               smtpmail-smtp-service 587
               starttls-use-gnutls t
               starttls-extra-arguments '("--insecure"))))

    (eval-after-load "org"
      '(progn
         ;; customize org-file locations
         (setq org-directory "~/Dropbox/org"
               org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org"
               org-mobile-directory "~/Dropbox/Apps/MobileOrg"
               my-refile (expand-file-name "~/Dropbox/org/refile.org")
               my-refile (expand-file-name "~/Dropbox/org/refile.org")
               my-kanban (expand-file-name "~/Dropbox/org/kanban.org")
               my-notes-file (expand-file-name "~/Dropbox/org/Notes.org")
               my-chinese-notes (expand-file-name "~/Dropbox/org/Chinese.org")
               my-agenda-files `(,(expand-file-name "~/Dropbox/org/")
                                 ,(expand-file-name "~/Dropbox/org/Calico/")))

                                        ;(require 'org-octopress)

         ;; configure octopress
         ;; (setq org-octopress-directory-top       "~/src/blog-stuff/blog/source"
         ;;       org-octopress-directory-posts     "~/src/blog-stuff/blog/source/_posts"
         ;;       org-octopress-directory-org-top   "~/src/blog-stuff/blog/source"
         ;;       org-octopress-directory-org-posts "~/src/blog-stuff/blog/source/blog"
         ;;       org-octopress-setup-file          "~/src/blog-stuff/blog/setup.org")

         ;; other PDF processing
         ;; org-latex-pdf-process
         ;; '("/Library/TeX/Distributions/TeXLive-2012.texdist/Contents/Programs/texbin/pdflatex -interaction nonstopmode -output-directory %o %f"
         ;;   "/Library/TeX/Distributions/TeXLive-2012.texdist/Contents/Programs/texbin/pdflatex -interaction nonstopmode -output-directory %o %f"
         ;;   "/Library/TeX/Distributions/TeXLive-2012.texdist/Contents/Programs/texbin/pdflatex -interaction nonstopmode -output-directory %o %f")
         ))

    (require 'grr)
    (defun my-grr-notify-build-args-list (args)
      (let* ((args-plist (list :name "--name"
                               :sticky "--sticky"
                               :application "--appIcon"
                               :icon "--iconpath"
                               :image "--image"
                               :priority "--priority"
                               :id "--identifier"
                               :identifier "--identifier"
                               :url "--url"))
             (additional-args (mapcar (lambda (x)
                                        (if (symbolp x)
                                            (plist-get args-plist x)
                                          x))
                                      args)))
        (append (list "grr" nil grr-command)
                additional-args)))

    (eval-after-load "nrepl"
      '(progn
         (require 'grr)

         (defadvice clojure-test-echo-results
           (after my-growl-clojure-test-results () activate)
           (if (or (< 0 clojure-test-failure-count)
                   (< 0 clojure-test-error-count))
               (my-notify-fail "clojure-test"
                               (format "FAIL!\nRan %s tests. %s failures, %s errors."
                                       clojure-test-count
                                       clojure-test-failure-count
                                       clojure-test-error-count))
             (my-notify-pass "clojure-test"
                             (format "Pass.\nRan %s tests. %s failures, %s errors."
                                     clojure-test-count clojure-test-failure-count
                                     clojure-test-error-count))))))))
