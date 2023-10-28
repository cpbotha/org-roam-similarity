;; without this, I was getting void-function org-all-archive-files
(require 'org-archive)
(require 'org-roam)
;; emacs-lisp code to iterate over org-roam-node-list
;; for each node, open the file, goto-char to "point"
;; then use (org-export-as 'md (org-at-heading-p) nil t)
;; to export as markdown, and save that to a file named after the node's "id"
;; write the code below:

(defun export-node-as-markdown (node)
  "Export the given node as markdown and save it in a file."
  (let* ((file (org-roam-node-file node))
         (id (org-roam-node-id node))
         (point (org-roam-node-point node))
         (title (org-roam-node-title node))
         ;; silence: Need absolute ‘org-attach-id-dir’ to attach in buffers without filename
         (org-attach-directory "/tmp"))
    
    ;;(message "%s - %s" file title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char point)
      ;; TODO: if source buffer is already md, don't export!
      (let* ((md (org-export-as 'md (org-at-heading-p) nil t )))
        (with-temp-file (concat id ".md")
          (insert (format "---\ntitle: %s\n---\n" title))
          (insert md))))))

;; the exporter breaks on includes
(cl-letf (((symbol-function 'org-export-expand-include-keyword) #'(lambda())))
  (let ((default-directory "/tmp")
        (node-list (org-roam-node-list)))
    (message "======> exporting %d nodes" (length node-list))
    (dolist (node node-list)
      (export-node-as-markdown node))
    )
  (message "DONE.")

  )

