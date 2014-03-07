(when (eq system-type 'windows-nt)
  (progn

    (require 's)

    (defvar consolas "-outline-Consolas-normal-r-normal-normal-18-97-96-96-c-*-iso8859-1")

    (set-default-font consolas)

    (setq browse-url-browser-function
          'browse-url-default-windows-browser)

    (setq projectile-indexing-method 'native
          projectile-enable-caching t)

    ;; customize org-file locations
    (setq org-directory "C:/Users/cbilson/Documents"
          my-refile "C:/Users/cbilson/Documents/Log.org"
          my-kanban "C:/Users/cbilson/Documents/Log.org"
          my-notes-file "C:/Users/cbilson/Documents/Log.org"
          my-agenda-files '("C:/Users/cbilson/Documents/"))

    ;; windows aspell stuff
    (add-to-list 'exec-path "C:\\Program Files (x86)\\Aspell\\bin")
    (setq ispell-program-name "aspell.exe")
    (setq ispell-aspell-data-dir "C:\\Program Files (x86)\\Aspell\\data")
    (setq ispell-aspell-dict-dir "C:\\Program Files (x86)\\Aspell\\dict")
    (setq ispell-personal-dictionary "C:/Users/cbilson/.ispell")
    (require 'ispell)
    (add-to-list 'ispell-local-dictionary-alist '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
                                                  ("-B")
                                                  nil iso-8859-1))

    ;; TFS stuff
    (defvar *tfs-server* "http://hqvm32:8080")
    (defvar *tfs-root* "/tfs/Calico/EIS")

    (defun org-open-tfs (item)
      (browse-url (concat *tfs-server* *tfs-root* "/_workItems#_a=edit&id=" item)))

    (eval-after-load "org"
      '(progn
         (org-add-link-type "tfs" 'org-open-tfs))))

;;; Other stuff to do on windows boxes
  ;; (setq ag-executable "C:/Users/cbilson/AppData/Local/scoop/apps/ag/0.18.1-1106/ag.exe")
  (defvar powershell-exe "C:\\windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe")

  (defun powershell-path ()
    (let* ((command (s-join " " (list powershell-exe
                                      "-ExecutionPolicy" "Unrestricted"
                                      "-Command" "$env:PATH")))
           (path (shell-command-to-string command))
           (cleaned-path (s-trim path)))
      cleaned-path))

  ;; (funcall 'powershell-path)

  (defun set-path (path-fun)
    (let* ((path (funcall path-fun))
           (emacsy-path  (s-replace "\\" "/" path)))
      (setenv "PATH" path)
      (set 'exec-path (split-string emacsy-path ";"))))

  (set-path 'powershell-path)

  (setq ag-executable "C:/Users/cbilson/appdata/local/scoop/apps/ag/0.18.1-1106/ag.exe")

  ;; (require 'ag)
  ;; (ag/search "foo" "D:\\src")
  ;; (getenv "PATH")
  
;;; 1) Install Chocolatey
;;; 2) cinst Everything

  (setq twittering-curl-program "C:/Chocolatey/lib/cmder.portable.1.1.1/tools/cmder/vendor/msysgit/bin/curl.exe")

  )
