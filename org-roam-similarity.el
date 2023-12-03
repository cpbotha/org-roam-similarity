;;; org-roam-similarity.el -- show similar org-roam nodes based on embeddings

;; Copyright (C) 2023 Charl P. Botha

;; Author: J. R. Hacker <jrh@example.com>
;; Version: 1.0
;; Package-Requires: ((org-roam "2.2.2"))
;; Keywords: org, similarity
;; URL: https://github.com/cpbotha/org-roam-similarity

;; without this, I was getting void-function org-all-archive-files
(require 'org-archive)
(require 'org-roam)

;; this takes less than a second for my 1700 nodes
;; markdown export above takes a few minutes in contrast
;; the price is that we're getting some orgmode cruft as part of the deal
(defun ors--export-node-as-native (node)
  "Export the given node as is (md file, org file, org subtree) and save it in a file."
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
      (let* ((txt (if (or (string= "md" (file-name-extension file)) (= point 1))
                      (buffer-string)
                    (progn
                      (let* ((element (org-element-at-point))
                             (begin (org-element-property :begin element))
                             (end (org-element-property :end element)))
                        (buffer-substring begin end)) )
                    )))
        (with-temp-file (concat id ".txt")
          (insert txt))))))


;;;###autoload
(defun ors-export-org-roam-nodes (target-dir)
  ;; the exporter breaks on includes, so here we temporarily blot out that function
  (cl-letf (((symbol-function 'org-export-expand-include-keyword) #'(lambda()))
            ((symbol-function #'run-mode-hooks) #'ignore) ;; for speed
            )
    (let ((default-directory target-dir)
          (node-list (org-roam-node-list)))
      (message "======> exporting %d nodes" (length node-list))
      (dolist (node node-list)
        (ors--export-node-as-native node)
        ))
    (message "DONE.")))


