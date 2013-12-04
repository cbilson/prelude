;;; custom -- my emacs customizations
;;; Commentary:
;;; Code:

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; get rid of some prelude keybindings
(global-unset-key (kbd "C-c RET"))

(eval-when-compile (require 'cl))


;;;
;;; Packages
;;;

;; base packages I want on all machines
(prelude-require-packages
 '(cider clojure-cheatsheet clojure-mode clojure-snippets clojurescript-mode
   coffee-mode csv-mode ctags ctags-update cyberpunk-theme
   dired-details
   editorconfig elein elisp-slime-nav emmet-mode ensime ess
   feature-mode
   git-gutter+ google-c-style goto-chg
   idle-highlight-mode iedit
   js-comint
   kibit-mode
   loccur
   markdown-mode multiple-cursors
   org org-magit
   paredit powershell-mode pp-c-l
   rbenv
   s scala-mode2 slamhound smex
   twittering-mode
   wrap-region
   w3m))

;; some modes that don't have working autoloads of their own
(autoload 'loccur-current "loccur")
(autoload 'clojure-mode "clojure-mode")
(autoload 'dired-details-install "dired-details")
(autoload 'google-make-newline-indent "google-c-style")
(autoload 'google-set-c-style "google-c-style")
(autoload 'powershell-mode "powershell-mode")


;;;
;;; Global Settings
;;;
(global-hl-line-mode -1)
(global-auto-revert-mode 1)
(global-undo-tree-mode 1)
(global-subword-mode 1)
(global-git-gutter+-mode)
(ido-everywhere t)
(idle-highlight-mode +1)
(menu-bar-mode -1)
(display-time-mode +1)
(helm-mode +1)
(blink-cursor-mode +1)
(load-theme 'cyberpunk t)

;;; projectile
(projectile-global-mode +1)
(setq projectile-enable-caching t
      projectile-completion-system 'grizzl)

(setq abbrev-mode t
      auto-revert-verbose nil
      c-basic-offset 2
      debug-on-error t
      global-auto-revert-non-file-buffers t
      initial-scratch-message nil
      ispell-dictionary "en_US"
      save-abbrevs nil
      visible-bell t
      whitespace-line-column 80
      whitespace-style '(trailing tabs lines-tail))

;;; auto-mode
(setq auto-mode-alist
      (remove (rassoc 'asm-mode auto-mode-alist) auto-mode-alist))

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.asm$" . nasm-mode)
                ("\\.aspx$" . html-mode)
                ("\\.cljs$" . clojurescript-mode)
                ("\\.config$" . xml-mode)
                ("\\.cs$" . csharp-mode)
                ("\\.cshtml$" . html-mode)
                ("\\.csman$" . xml-mode)
                ("\\.rd$" . xml-mode)
                ("\\.rdsc$" . xml-mode)
                ("\\.rels$" . xml-mode)
                ("\\.m$" . octave-mode)
                ("\\.ps1$" . powershell-mode)
                ("\\.R$" . r-mode)
                ("\\.r$" . r-mode)
                ("\\.spark$" . html-mode))))

;;; Backups
(setq backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;; Remembering where I was
(setq-default save-place t)
(savehist-mode t)

;;; Aliases
(defalias 'plp 'package-list-packages)
(defalias 'redo 'undo-tree-redo)

;;; Abbreviations
(define-abbrev-table 'my-abbrevs
  '(("8in" "∈")
    ("8nin" "∉")
    ("8inf" "∞")))

;;; Mini-Buffer
(defun my-minibuffer-setup-hook ()
  (when (boundp 'my-key-minor-mode)
    (my-keys-minor-mode 0)))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;; some vars I use
(defvar my-refile nil "Where I capture things")
(defvar my-kanban nil "My personal kanban")
(defvar my-notes-file nil "Where to store org-captured notes and other stuff")
(defvar my-chinese-notes nil)
(defvar my-agenda-files nil)


;;;
;;; Key bindings
;;;
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(put 'set-goal-column 'disabled nil)

;; I never use ansi-term and used to hit C-c t by accident all the time
(define-key prelude-mode-map (kbd "C-c t") 'eshell)

(defvar my-keys-minor-mode-map (make-keymap) "keymap for my overrides")
(define-key my-keys-minor-mode-map (kbd "C-c C-r") 'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x C-i") 'imenu)
(define-key my-keys-minor-mode-map (kbd "C-h a") 'apropos)
(define-key my-keys-minor-mode-map (kbd "C-c q") 'join-line)
(define-key my-keys-minor-mode-map (kbd "C-c C-M-j") 'cider-jack-in)
(define-key my-keys-minor-mode-map (kbd "C-c c") 'org-capture)
(define-key my-keys-minor-mode-map (kbd "C-c a") 'org-agenda)
(define-key my-keys-minor-mode-map (kbd "C-o") 'loccur-current)
(define-key my-keys-minor-mode-map (kbd "C-M-o") 'loccur)
(define-key my-keys-minor-mode-map (kbd "C-S-o") 'loccur-previous-match)
(define-key my-keys-minor-mode-map (kbd "C-c ;") 'iedit-mode)
(define-key my-keys-minor-mode-map (kbd "C-c C-;") 'iedit-mode-from-isearch)
(define-key my-keys-minor-mode-map (kbd "C-C m'") 'mc/edit-lines)
(define-key my-keys-minor-mode-map (kbd "C-c M-:") 'set-rectangular-region-anchor)
(define-key my-keys-minor-mode-map (kbd "M-`") 'goto-last-change)
(define-key my-keys-minor-mode-map (kbd "C-c -") 'er/contract-region)
(define-key my-keys-minor-mode-map (kbd "C-c C-f") 'browse-url)
(define-key my-keys-minor-mode-map (kbd "C-c F") 'my-recentf-ido-find-file-other-window)
(define-key my-keys-minor-mode-map (kbd "C-c |") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-c _") 'split-window-below)

(define-key helm-find-files-map (kbd "C-c DEL") 'helm-ff-run-toggle-auto-update)

(define-minor-mode my-keys-minor-mode
  "Minor mode for my keybindings"
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;;;
;;; Code Editing
;;;

;;; prog-mode
(eval-after-load "simple"
  '(progn
     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
     (add-hook 'prog-mode-hook 'turn-off-auto-fill)
     (add-hook 'prog-mode-hook 'guru-mode)
     (add-hook 'prog-mode-hook 'whitespace-mode)
     (add-hook 'prog-mode-hook 'idle-highlight-mode)
     (add-hook 'prog-mode-hook 'rainbow-mode)
     (add-hook 'prog-mode-hook 'subword-mode)

     (define-key prog-mode-map (kbd "M-;") 'comment-dwim)
     (define-key prog-mode-map (kbd "RET") 'newline-and-indent)))

;;; Makefiles
(eval-after-load "makefile-mode"
  '(progn
     (add-hook 'write-file-hooks 'my-tabify-buffer)))

;;; magit
(eval-after-load "magit"
  '(progn

     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defun magit-quit-session ()
       "Restores the previous window configuration and kills the magit buffer"
       (interactive)
       (kill-buffer)
       (jump-to-register :magit-fullscreen))

     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

;;; lisp stuff
(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-C-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-C-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-C-{") 'paredit-wrap-curly)
     (define-key paredit-mode-map (kbd "M-;") 'paredit-comment-dwim)
     (define-key paredit-mode-map (kbd "C-c }") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-c {") 'paredit-backward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c (") 'paredit-backward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-R") 'paredit-splice-sexp-killing-backward)))

(eval-after-load "lisp-mode"
  '(progn
     (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
     (define-key lisp-interaction-mode-map (kbd "C-x C-e") 'eval-print-last-sexp)
     (define-key emacs-lisp-mode-map (kbd "M-;") 'paredit-comment-dwim)
     (define-key emacs-lisp-mode-map (kbd "RET") 'paredit-newline)))

(eval-after-load "ielm"
  '(progn
     (add-hook 'ielm-mode-hook 'paredit-mode)
     (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

;;; Clojure Stuff
(eval-after-load "clojure-mode"
  '(progn
     (add-hook 'clojure-mode-hook 'paredit-mode)
     (define-key clojure-mode-map (kbd "M-;") 'paredit-comment-dwim)
     (define-key clojure-mode-map (kbd "RET") 'paredit-newline)
     (defun helm-clojure-headlines ()
       "Display headlines for the current Clojure file."
       (interactive)
       (helm :sources '(((name . "Clojure Headlines")
                         (volatile)
                         (headline "^[;(]")))))

     (defun elein-deps ()
       (interactive)
       (elein-run-cmd "deps"))

     ;; (require 'clojure-cheatsheet)
     ))

(eval-after-load "clojurescript"
  '(progn
     (defun clojurescript-run-cljsbuild (dir)
       (interactive (list default-directory))
       (when (get-buffer "*lein-cljsbuild*")
         (kill-buffer "*lein-cljsbuild*"))
       (let* ((cmd (format "cd %s && lein cljsbuild auto" dir))
              (proc (start-process-shell-command
                     "leincljsbuild" "*lein-cljsbuild*" cmd)))
         (message (format "dir: %s" dir))
         (set-process-filter proc
                             (lambda (process output)
                               (with-current-buffer (process-buffer process)
                                 (save-excursion
                                   (goto-char (point-max))
                                   (insert output)))
                               (cond
                                ((string-match "That's not a task" output)
                                 (let ((debug-on-error t))
                                   (error "Couldn't run `lein cljsbuild'. Wrong directory?")))
                                ((string-match "\\(Deleting\\|Compiling\\) ClojureScript" output)
                                 (message "lein cljsbuild: %s files" (match-string 1 output)))
                                ((string-match "Failed!" output)
                                 (message "lein cljsbuild: Compilation FAILED")
                                 (ding))
                                ((string-match "compiled \".+\" in \\(.+\\) seconds" output)
                                 (message "lein cljsbuild: Finished in %s seconds"
                                          (match-string 1 output))))))
         (display-buffer (get-buffer "*lein-cljsbuild*"))
         (message "Running lein cljsbuild...")))

     (defun clojurescript-repl-listen (dir)
       (interactive (list default-directory))
       (inferior-lisp (format "cd %s && lein trampoline cljsbuild repl-listen" dir)))
     
     (add-hook 'clojurescript-mode-hook 'paredit)
     (define-key clojure-mode-map (kbd "M-;") 'paredit-comment-dwim)))

(eval-after-load "cider"
  '(progn

     (setq cider-use-pretty-printing t
           cider-popup-stacktraces t
           cider-repl-popup-stacktraces t
           cider-auto-select-error-buffer nil
           cider-repl-display-in-current-window t
           cider-repl-wrap-history t
           cider-repl-history-file (expand-file-name "~/.emacs.d/personal/.cider-history")
           nrepl-hide-special-buffers t
           cider-repl-pop-to-buffer-on-connect nil)

     (add-hook 'cider-mode-hook 'paredit-mode)
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (add-hook 'cider-repl-mode-hook 'subword-mode)
     (add-hook 'cider-repl-mode-hook 'paredit-mode)
     (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
     (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)

     (add-to-list 'same-window-buffer-names "*cider*")

     ;; if the vendor directory has cider-inspect, set that up
     (let ((inspect-dir (expand-file-name  "cider-inspect" prelude-vendor-dir)))
       (when (file-exists-p inspect-dir)
         (add-to-list 'load-path inspect-dir)
         (require 'cider-inspect)
         (define-key cider-mode-map (kbd "C-c i") 'cider-inspect)))))

(eval-after-load "elein"
  '(progn
     (defun lein-test ()
       (interactive)
       (let ((output (get-buffer-create "*lein test*"))
             (errors (get-buffer-create "*lein test errors*")))
         (bury-buffer output)
         (bury-buffer errors)
         ;;(elein-burried-shell-command (concat elein-lein " test") buffer)
         ;;(elein-in-project-root (compile (concat elein-lein " test")))
         ;;(elein-run-cmd "test")
         (shell-command (concat elein-lein " test") output errors)))))

;; javadoc
;;(javadoc-add-roots "")

(eval-after-load "c-mode"
  '(progn
     (define-key c-mode-map (kbd "C-c C-k") 'compile)
     (add-hook 'c-mode-common-hook 'google-set-c-style)
     (add-hook 'c-mode-common-hook 'google-make-newline-indent)
     (setq compilation-read-command nil)))

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-base-map (kbd "C-c C-k") 'compile)
     (add-hook 'c-mode-common-hook 'google-set-c-style)
     (add-hook 'c-mode-common-hook 'google-make-newline-indent)
     (setq compilation-read-command nil)))

(eval-after-load "js"
  '(progn
     (add-hook 'js-mode-hook 'paredit-mode)
     (setq c-basis-offset 2)))

;;; python stuff
(eval-after-load "python-mode"
  '(progn
     (setq
      python-shell-interpreter "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code ""
      python-shell-completion-string-code
      "';'.join(__IP.complete('''%s'''))\n")))

;;; scala stuff
(eval-after-load "scala-mode"
  '(progn
     (require 'scala-mode2)
     ;;(require 'ensime)
     (defun my-scala-mode-hook ()
       (scala-mode-feature-install))

     ;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
     (add-hook 'scala-mode-hook 'my-scala-mode-hook)))

;;; powershell
(eval-after-load "powershell-mode"
  '(progn
     (setq
      powershell-indent 2
      powershell-continuation-indent 2)
     (define-key powershell-mode-map (kbd "RET") 'newline-and-indent)))

;;; csharp-mode
(eval-after-load 'csharp-mode
  '(progn
     (defun csharp-makefile-compile ()
       (interactive)
       (cd (locate-dominating-file default-directory "Makefile"))
       (compile "make test"))

     (define-key csharp-mode-map (kbd "C-c ,") 'w-unit-test)
     (define-key csharp-mode-map (kbd "C-c C-k") 'csharp-makefile-compile)
     (define-key csharp-mode-map (kbd "{") 'c-electric-brace)))

;;; 
;;; markup languages
;;;

;;; zencoding/emmet
(eval-after-load "emmet-mode"
  '(progn
     (setq emmet-preview-default nil
           emmet-indentation 2)))

(eval-after-load "sgml-mode"
  '(progn
     (add-hook 'sgml-mode-hook 'emmet-mode)
     (define-key sgml-mode-map (kbd "RET") 'newline-and-indent)))

(eval-after-load "nxml-mode"
  '(progn
     (add-hook 'nxml-mode-hook 'emmet-mode)
     (define-key nxml-mode-map (kbd "RET") 'newline-and-indent)))

(eval-after-load "css-mode"
  '(progn
     (add-hook 'css-mode-hook 'emmet-mode)))

(eval-after-load "html-mode"
  '(progn
     (add-hook 'html-mode-hook 'emmet-mode)))

;;; 
;;;  org-mode
;;; 
(eval-after-load "org"
  '(progn
     (add-hook 'org-mode-hook 'auto-fill-mode)

     (setq org-todo-keywords '((sequence "BACKLOG(b!)" "READY(r!)" "TODAY(t!)"
                                         "DOING(d!)" "|" "DONE(n!)" "CANCELLED(x!)"
                                         "BLOCKED(l!)"))
           org-list-allow-alphabetical t
           org-log-into-drawer t
           org-hide-leading-stars t
           org-refile-targets '((nil . (:level . 2)))
           org-clock-persist 'history
           org-src-fontify-natively t
           org-agenda-files my-agenda-files
           org-confirm-babel-evaluate nil
           org-default-notes-file my-notes-file)

     (setq org-capture-templates
           '(("i" "Interesting" entry (file+headline my-refile "Interesting")
              "* %T %^{Description}\n %x")
             ("t" "BACKLOG" entry (file+headline my-notes-file "Backlog")
              "* BACKLOG %?\n%U\n%i\n%a")
             ("p" "P<hone call" entry (file my-refile)
              "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
             ("c" "Chinese" entry (file+headline my-chinese-notes "Vocabulary")
              "* %i\n %?\n %a")))

     (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

     (defvar org-babel-default-header-args:clojure
       '((:results . "silent")))

     (setq org-capture-templates
           '(("i" "Interesting" entry (file+headline my-notes-file "Interesting")
              "* %T %^{Description}\n %a %i\n%x")
             ("t" "TODO" entry (file+headline my-notes-file "Tasks")
              "* BACKLOG %?\n %i\n %a")))

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((C . t)
        (clojure . t)
        (dot . t)
        (emacs-lisp . t)
        (gnuplot . t)
        (js . t)
        (ledger . t)
        (octave . t)
        (python . t)
        (R . t)
        (scala . t)
        (sh . t)))

     (add-to-list 'org-structure-template-alist
                  '("z" "#+name: expenses\n#+begin_src ledger :noweb yes\n?\n#+end_src"
                    "<src lang=\"ledger\">\n\n</src>"))))

(eval-after-load "ob-clojure"
  '(progn
     (defun org-babel-execute:clojure (body params)
       "Execute a block of Clojure code with Babel and CIDER."
       (require 'cider)
       (if (nrepl-current-connection-buffer)
           (let ((result (cider-eval (org-babel-expand-body:clojure body params))))
             (car (read-from-string (plist-get result :value))))
         (error "CIDER not connected!")))

     (add-hook 'org-src-mode-hook
               '(lambda ()
                  (set (make-local-variable 'nrepl-buffer-ns)
                       (with-current-buffer
                           (overlay-buffer org-edit-src-overlay)
                         nrepl-buffer-ns))))))

(eval-after-load "ledger-mode"
  '(progn
     (custom-set-faces
      '(ledger-font-xact-highlight-face ((t (:background "gray10"))) t)
      '(ledger-occur-xact-face ((t (:background "gray20"))) t))
     (setq ledger-use-iso-dates t)
     (custom-set-faces
      '(ledger-font-xact-highlight-face ((t (:background "gray10"))) t)
      '(ledger-occur-xact-face ((t (:background "gray20"))) t))
     (setq ledger-reports '(("bud" "ledger --budget --monthly reg '^Expenses'")
                            ("bal" "ledger -f %(ledger-file) bal")
                            ("reg" "ledger -f %(ledger-file) reg")
                            ("payee" "ledger -f %(ledger-file) reg @%(payee)")
                            ("account" "ledger -f %(ledger-file) reg %(account)")
                            ("budget" "ledger -f %{ledger-file} --budget --monthly reg '^Expenses' -p 'this month'")))
     (local-set-key (kbd "C-o") 'ledger-occur)))

;;;
;;; Other, Non-Developer Tools
;;;

;;;  google-translate
;; (defvar *my-language* "English")
;; (defvar *learning-language* "Chinese Traditional")

;; (defun my-translate-at-point ()
;;   (interactive)
;;   (google-translate-translate *learning-language* *my-language*
;;                               (current-word t)))

;; (global-set-key (kbd "<f1>") 'my-translate-at-point)

;;; twittering
(eval-after-load "twittering-mode"
  '(progn
     (setq twittering-use-master-password t)))

;;; w3m
(eval-after-load "w3m"
  '(progn
     (require 'org-w3m)))

;;; ERC
(eval-after-load "erc-mode"
  '(progn
     (require 'erc-track)
     (require 'erc-match)
     (require 'erc-ring)
     (require 'erc-fill)
     (require 'erc-netsplit)

     (setq erc-echo-notices-in-minibuffer-flag t
           erc-user-full-name "Chris Bilson"
           erc-part-reason-various-alist '(("^$" "Leaving"))
           erc-quit-reason-various-alist '(("^$" "Leaving"))
           erc-quit-reason 'erc-part-reason-various
           erc-part-reason 'erc-quit-reason-various
           erc-spelling-mode t
           erc-track-mode t
           erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                     "324" "329" "332" "333" "353" "477")
           erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
           erc-keywords '("keywords" "highlight" "username")
           erc-autojoin-channels-alist '(("freenode.net" "#clojure"))
           erc-ring-mode t
           erc-fill-mode t
           erc-netsplit-mode t
           erc-max-buffer-size 40000
           erc-scrolltobottom-mode t
           erc-kill-buffer-on-part t
           erc-truncate-buffer-on-save t)

     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
     (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)))

;;; IRC
(require 'netrc)
(defun irc-jack-in ()
  (interactive)
  (let* ((host "irc.freenode.net")
         (port 6697)
         (netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (hostentry (netrc-machine netrc host port port)))
    (erc-tls :server host :port port
             :nick "cbilson" :full-name "Chris Bilson"
             :password (netrc-get hostentry "password"))))

;;; GNUS
(eval-after-load "gnus"
  '(progn
     (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

     (setq gnus-use-adaptive-scoring t)
     (setq gnus-score-expiry-days 14)
     (setq gnus-default-adaptive-score-alist
           '((gnus-unread-mark)
             (gnus-ticked-mark (from 4))
             (gnus-dormant-mark (from 5))
             (gnus-saved-mark (from 20) (subject 5))
             (gnus-del-mark (from -2) (subject -5))
             (gnus-read-mark (from 2) (subject 1))
             (gnus-killed-mark (from 0) (subject -3))))
                                        ;(gnus-killed-mark (from -1) (subject -3))))
                                        ;(gnus-kill-file-mark (from -9999)))
                                        ;(gnus-expirable-mark (from -1) (subject -1))
                                        ;(gnus-ancient-mark (subject -1))
                                        ;(gnus-low-score-mark (subject -1))
                                        ;(gnus-catchup-mark (subject -1))))

     (setq gnus-score-decay-constant 1) ;default = 3
     (setq gnus-score-decay-scale 0.03) ;default = 0.05

     (setq gnus-decay-scores t) ;(gnus-decay-score 1000)

     (setq gnus-global-score-files
           '("~/gnus/scores/all.SCORE"))

     ;; all.SCORE contains:
     ;;(("xref"
     ;;  ("gmane.spam.detected" -1000 nil s)))
     (setq gnus-summary-expunge-below -999)
     (setq gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")
     (setq gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")

     (setq gnus-summary-same-subject "")
     (setq gnus-sum-thread-tree-root "")
     (setq gnus-sum-thread-tree-single-indent "")
     (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
     (setq gnus-sum-thread-tree-vertical "|")
     (setq gnus-sum-thread-tree-single-leaf "`-> ")

     (setq gnus-plugged nil
           gnus-use-cache t
           gnus-asynchronous t)

     (setq message-kill-buffer-on-exit t)
     (add-hook 'message-mode-hook 'turn-on-auto-fill)
     (add-hook 'message-sent-hook 'gnus-score-followup-article)
     (add-hook 'message-sent-hook 'gnus-score-followup-thread)
     (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
     (setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")
     (setq gnus-sorted-header-list '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:" "^X-Newsreader:"))

     (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
     (setq spam-directory "~/gnus/spam/")

     (setq gnus-spam-process-newsgroups
           '(("^gmane\\."
              ((spam spam-use-gmane)))))

     (setq gnus-select-method
           '(nntp "news.eternal-september.org"
                  (nntp-open-connection-function nntp-open-tls-stream)
                  (nntp-port-number 563)
                  (nntp-address "news.eternal-september.org")))

     (add-to-list 'gnus-secondary-select-methods
                  '(nntp "news.gmane.org"
                         (nntp-open-connection-function nntp-open-tls-stream)
                         (nntp-port-number 563)
                         (nntp-address "news.gmane.org")))

     (add-to-list 'gnus-secondary-select-methods
                  '(nntp "news.gwene.org"
                         (nntp-open-connection-function nntp-open-tls-stream)
                         (nntp-port-number 563)
                         (nntp-address "news.gwene.org")))

     (setq nntp-record-commands t)))

;;; mu4e
(eval-after-load "message-mode"
  '(progn
     (add-hook 'message-mode-hook 'flyspell-mode)))

;;; dired
(eval-after-load "dired"
  '(progn
     (dired-details-install)
     (defun dired-zip-files (zip-file)
       "Create an archive containing the marked files."
       (interactive "sEnter name of zip file: ")

       ;; create the zip file
       (let ((zip-file (if (string-match ".zip$" zip-file)
                           zip-file
                         (concat zip-file ".zip"))))
         (shell-command
          (concat "zip "
                  zip-file
                  " "
                  (my-concat-string-list
                   (mapcar
                    '(lambda (filename)
                       (file-name-nondirectory filename))
                    (dired-get-marked-files))))))

       (revert-buffer)

       ;; remove the mark on all the files  "*" to " "
       (dired-change-marks 42 ?\040)
       ;; mark zip file
       (dired-mark-files-regexp (filename-to-regexp zip-file)))

     (define-key dired-mode-map "z" 'dired-zip-files)))

(eval-after-load "dired-aux"
  '(progn
     (add-to-list 'dired-compress-file-suffixes
                  '("\\.zip\\'" ".zip" "unzip"))))

;;; eshell
(eval-after-load "eshell"
  '(progn
     (defun my-eshell-hook ()
       (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete))

     (add-hook 'eshell-mode-hook 'my-eshell-hook)))

;;; ack
(eval-after-load "ack-and-a-half"
  '(progn
     (setq ack-and-a-half-arguments "")))


;;; normal emacs customization stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(ansi-term-color-vector [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(canlock-password "cbdcf02760b26ab76b6e750b73c6674d6820b554")
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes (quote ("636ecbf1091fbc99d95526d7a3a4810d1ccb58997e58bd3184863821303553f3" "5c674d297206a2494eff9bf650a2ffbb8261d5a2ee77563b8a6530525fec5b6d" "31bfef452bee11d19df790b82dea35a3b275142032e06c6ecdc98007bf12466c" "8bb1e9a22e9e9d405ca9bdf20b91301eba12c0b9778413ba7600e48d2d3ad1fb" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683" "7a2c92b6267b84ae28a396f24dd832e29a164c1942f1f8b3fe500f1c25f8e09d" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "b8f561a188a77e450ab8a060128244c81dea206f15c1152a6899423dd607b327" "1c1e6b2640daffcd23b1f7dd5385ca8484a060aec901b677d0ec0cf2927f7cde" "f48b43277382ee56a9e3c5d08b71c3639f401f6338da00039dcac3c21c0811b5" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" "bad832ac33fcbce342b4d69431e7393701f0823a3820f6030ccc361edd2a4be4" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "450b29ed22abeeac279b7eeea592f4eea810105737716fc29807e1684e729c55" "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "337047491f7db019df2ba54483408d7d7faea0bda61e4c4f5e8cf2f4e3264478" "865d6cb994f89c13b2d7e5961df4eabeea12494583c240c8fe9a788d0f4ee12c" "d921083fbcd13748dd1eb638f66563d564762606f6ea4389ea9328b6f92723b7" "66bd7fc2ed32703a332d05f5d2af5c30c12ff4e729d77d8271b91d6f6f7e15fc" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "78cfbd96775588c06c4fff22573aaa5fa762ca2b8eda43cb964b7739194ed3c1" "b674ccba78eb688bea71ea8a1fd2782fcd69bd462e2504008903b5b6e018b480" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "465be5317c7d95a84e376e095c21242f4f2ad75692ed806dcbb6fe27078260f1" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "5ce9c2d2ea2d789a7e8be2a095b8bc7db2e3b985f38c556439c358298827261c" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "88b663861db4767f7881e5ecff9bb46d65161a20e40585c8128e8bed8747dae5" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "5dfacaf380068d9ed06e0872a066a305ab6a1217f25c3457b640e76c98ae20e6" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d7f1c86b425e148be505c689fc157d96323682c947b29ef00cf57b4e4e46e6c7" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "30f861ee9dc270afc2a9962c05e02d600c998905433c8b9211dc2b33caa97c51" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "cc26d05641be64f11c7da487db926e70c7b537e5c2c668d57b197a2ea1a9cc02" "ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" default)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#383838")
 '(foreground-color "#cccccc")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#29282E")
 '(main-line-color2 "#292A24")
 '(main-line-separator-style (quote chamfer))
 '(org-agenda-files (quote ("/Users/cbilson/Dropbox/org/Agenda.org" "/Users/cbilson/Dropbox/org/Chinese.org" "/Users/cbilson/Dropbox/org/Finances.org" "/Users/cbilson/Dropbox/org/Notes.org" "/Users/cbilson/Dropbox/org/kanban.org" "/Users/cbilson/Dropbox/org/refile.org")))
 '(powerline-color1 "#29282E")
 '(powerline-color2 "#292A24")
 '(safe-local-variable-values (quote ((nrepl-buffer-ns . "notes.core"))))
 '(send-mail-function (quote mailclient-send-it))
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

