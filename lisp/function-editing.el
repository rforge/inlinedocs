(defun ess-inlinedocs-add-example ()
  "add example template using structure() and leave the cursor in
  the new example function body"
  (interactive)
  (save-excursion
    (let* ((beg-end (ess-end-of-function))
	   (beg (nth 0 beg-end))
	   (end (nth 1 beg-end))
	   name)
      (goto-char beg)
      (re-search-forward "function(" nil t)
      (replace-match "structure(function(")
      (goto-char end)
      (re-search-forward "}" nil t)
      (replace-match "},ex=function(){\n  \n})"))))

(defun ess-inlinedocs-format-args ()
  "break function arguments apart and add blank inlinedocs comments"
  (interactive)
)

