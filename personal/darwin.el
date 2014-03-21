
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



