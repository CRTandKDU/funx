;;; Runtime SECD client

;;; Commands
(defun secd-run (file env)
  "Runs control list `file.fasl' into result file `file.out'" 
  (let* ((clist (with-temp-buffer
		  (insert-file-contents file)
		  (buffer-string)))
	 )
    (with-temp-file (format "%s.out" (file-name-sans-extension file))
      (insert
       (format "(push \"C:/Users/jmc/Documents/code/funx\" load-path)\n(require 'secd-exec)\n(setq res (secd-cycle nil '%s '%s nil))" env clist))
      (let ((ignore (eval-buffer)))
	(erase-buffer)
	(insert (format "%s" res))
	)
      )
    )
  )


(provide 'secd-run)
