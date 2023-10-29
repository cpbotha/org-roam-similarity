;; without this, I was getting void-function org-all-archive-files
(require 'org-archive)
(require 'org-roam)

;; emacs-lisp code to iterate over org-roam-node-list
;; for each node, open the file, goto-char to "point"
;; then use (org-export-as 'md (org-at-heading-p) nil t)
;; to export as markdown, and save that to a file named after the node's "id"
(defun export-node-as-markdown (node)
  "Export the given node as markdown and save it in a file."
  (let* ((file (org-roam-node-file node))
         (id (org-roam-node-id node))
         (point (org-roam-node-point node))
         (title (org-roam-node-title node))
         (file-mtime (org-roam-node-file-mtime node))
         ;; silence "Need absolute ‘org-attach-id-dir’ to attach in buffers without filename" error
         (org-attach-directory "/tmp"))
    
    ;;(message "%s - %s" file title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char point)
      ;; if source buffer is already md, just copy over the whole thing
      ;; if not, export the current node, which could be a heading within an org file
      (let* ((md (if (string= "md" (file-name-extension file))
                     (buffer-string)
                   (org-export-as 'md (org-at-heading-p) nil t ))))
        (with-temp-file (concat id ".md")
          ;; add yaml frontmatter that can be parsed out by python bits
          ;; escape "-characters in title
          ;; store mtime as iso8601 timestamp
          (insert (format "---\ntitle: \"%s\"\nmtime: %s\n---\n"
                          (replace-regexp-in-string "\"" "\\\\\"" title)
                          (format-time-string "%FT%T%z" file-mtime)))
          (insert md))))))

;; the exporter breaks on includes, so here we temporarily blot out that function
(cl-letf (((symbol-function 'org-export-expand-include-keyword) #'(lambda())))
  (let ((default-directory "/tmp/bleh/")
        (node-list (org-roam-node-list)))
    (message "======> exporting %d nodes" (length node-list))
    (dolist (node node-list)
      (export-node-as-markdown node))
    )
  (message "DONE."))


;; scratchpad
;;(org-roam-id-find "4063BD96-06F1-4529-96D1-32C0CBC6F4BA")
;;(org-roam-node-find)
