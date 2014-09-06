(when (eq system-type 'windows-nt)
  (progn

    (require 's)

    (setq home-directory
          (concat (getenv "HOMEDRIVE")
                  (file-name-as-directory (getenv "HOMEPATH"))))

    ;; customize org-file locations
    (setq one-drive-directory
          (cond
           ((string= system-name "CBILSON-MB") (concat home-directory "OneDrive"))
           ((string= system-name "CBILSON-WS") (concat home-directory "SkyDrive"))))

    (setq chocolatey-directory (file-name-as-directory (getenv "ChocolateyInstall"))
          chocolatey-bin-directory (concat chocolatey-directory "bin"))

    (setq org-directory (concat (file-name-as-directory one-drive-directory) "org")
          my-note-file (concat (file-name-as-directory org-directory) "Log.org")
          my-refile my-notes-file
          my-kanban my-notes-file
          my-agenda-files (list org-directory))

    (defvar consolas "-outline-Consolas-normal-r-normal-normal-18-97-96-96-c-*-iso8859-1")
    (set-default-font consolas)

    (setq git-gutter+-git-executable "C:/git/bin/git.exe")

    (eval-after-load "twittering-mode"
      '(progn
         (setq twittering-use-master-password nil)))

    (setq browse-url-browser-function
          'browse-url-default-windows-browser)

    (setq projectile-indexing-method 'native
          projectile-enable-caching t)

    ;; windows aspell stuff
    (add-to-list 'exec-path "C:\\Tools\\Aspell\\bin")
    (setq ispell-program-name "C:\\Tools\\Aspell\\bin\\aspell.exe")
    (setq ispell-aspell-data-dir "C:\\Tools\\Aspell\\data")
    (setq ispell-aspell-dict-dir "C:\\Tools\\Aspell\\dict")
    (setq ispell-personal-dictionary (concat home-directory ".ispell"))
    (require 'ispell)
    (add-to-list 'ispell-local-dictionary-alist '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
                                                  ("-B")
                                                  nil iso-8859-1))

    (setq delete-by-moving-to-trash t)

    ;; TFS stuff
    ;; (defvar *tfs-server* "http://hqvm32:8080")
    ;; (defvar *tfs-root* "/tfs/Calico/EIS")

    ;; (defun org-open-tfs (item)
    ;;   (browse-url (concat *tfs-server* *tfs-root* "/_workItems#_a=edit&id=" item)))

    ;; (eval-after-load "org"
    ;;   '(progn
    ;;      (org-add-link-type "tfs" 'org-open-tfs)))


    ;;; Other stuff to do on windows boxes
    ;; (setq ag-executable "C:/Users/cbilson/AppData/Local/scoop/apps/ag/0.18.1-1106/ag.exe")
    ;; (defvar powershell-exe "C:\\windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe")

    ;; (defun powershell-path ()
    ;;   (let* ((command (s-join " " (list powershell-exe
    ;;                                     "-ExecutionPolicy" "Unrestricted"
    ;;                                     "-Command" "$env:PATH")))
    ;;          (path (shell-command-to-string command))
    ;;          (cleaned-path (s-trim path)))
    ;;     cleaned-path))

    ;; (funcall 'powershell-path)

    ;; (defun set-path (path-fun)
    ;;   (let* ((path (funcall path-fun))
    ;;          (emacsy-path  (s-replace "\\" "/" path)))
    ;;     (setenv "PATH" path)
    ;;     (set 'exec-path (split-string emacsy-path ";"))))

    ;; (set-path 'powershell-path)

    ;; (require 'ag)
    ;; (ag/search "foo" "D:\\src")
    ;; (getenv "PATH")

    (setq twittering-curl-program (concat chocolatey-bin-directory "curl.exe"))

    (eval-after-load "geiser"
      '(progn
         (setq geiser-racket-binary "C:\\Program Files\\Racket\\Racket.exe")))))
