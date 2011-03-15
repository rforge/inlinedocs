(defvar ess-inlinedocs-example-start "},ex=function(){\n  "
  "text to insert for inlinedocs examples")

(defun ess-inlinedocs-add-example ()
  "add example template using structure() and leave the cursor in
  the new example function body"
  (interactive)
  (save-excursion
    (let* ((beg-end (ess-end-of-function))
	   (beg (nth 0 beg-end))
	   (end (nth 1 beg-end))
	   (REP (concat ess-inlinedocs-example-start "\n})"))
	   name)
      (goto-char beg)
      (re-search-forward "\\(function\\)" nil t)
      (replace-match "structure(\\1")
      (goto-char end)
      (re-search-forward "}" nil t)
      (replace-match REP)))
  (re-search-forward ess-inlinedocs-example-start))
 
(defvar ess-inlinedocs-comment-prefix "\n### \n"
  "what to insert after a argument or description")

(defvar ess-inlinedocs-arg-regexp "\\([^,)\n]+\\)"
  "regexp used to match argument names")

(defun ess-inlinedocs-format-args ()
  "break function arguments apart and add blank inlinedocs comments"
  (interactive)
  (ess-beginning-of-function)
  (let ((arg-regexp (concat "\\([ (,]\\)" ess-inlinedocs-arg-regexp "\\(,\\)")))
    (while (let ((end (save-excursion (nth 1 (ess-end-of-function)))))
	     (re-search-forward arg-regexp end t))
    (replace-match (concat "\\1\\2\\3" ess-inlinedocs-comment-prefix " "))
    (beginning-of-line)))
  (re-search-forward (concat ess-inlinedocs-arg-regexp "\\(){\\)"))
  (replace-match (concat "\\1" ess-inlinedocs-comment-prefix " \\2"))
  (ess-beginning-of-function)
  (re-search-forward "\\(function\\)" nil t)
  (replace-match (concat "\\1" ess-inlinedocs-comment-prefix)) ;description
  (backward-char))