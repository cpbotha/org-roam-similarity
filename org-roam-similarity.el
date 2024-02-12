;;; org-roam-similarity.el -- show similar org-roam nodes based on embeddings

;; Copyright (C) 2023 Charl P. Botha

;; Author: Charl P. Botha <cpbotha@vxlabs.com>
;; Version: 1.0
;; Package-Requires: ((org-roam "2.2.2"))
;; Keywords: org, similarity
;; URL: https://github.com/cpbotha/org-roam-similarity

;; TODO: if region selected, then only search for that region

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
  (interactive "DSelect org-roam nodes txt export directory: ")
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


;; add function to org-roam-mode-sections (by default it has two: backlinks and reflinks)
(require 'url)
(require 'json)

(defun ors--get-similar-nodes (text)
  "Return list of node,score representing most similar nodes to TEXT."
  (let* ((url "http://localhost:3814/similar/")
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         ;; have to utf-8 encode json-encode's result
         ;; see https://lists.gnu.org/archive/html/emacs-devel/2020-06/msg00515.html
         (url-request-data (encode-coding-string (json-encode `(("text" . ,text))) 'utf-8))
         (response-buffer (url-retrieve-synchronously url nil nil 5))
         json-array)
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      ;; this is a vector of vectors
      (setq json-array (json-read))
      (kill-buffer))
    ;; json-array is a vector with nested two-element (id, similarity-value) vectors
    ;; we mapcar over it, converting the outer vec to list, while we convert each
    ;; inner 2-element vector to a list with (append v nil)
    ;; first element extraction from vector would have been: (aref v 0)
    (mapcar (lambda (v) (append v nil)) json-array)))

;;;###autoload
(cl-defun org-roam-node-find-similar (&optional other-window initial-input pred &key templates)
  "Find and open org-roam nodes that are similar to the current region or buffer.
This is otherwise identical to `org-roam-node-find'."
  (interactive current-prefix-arg)
  (let* ((region-or-buffer (if (use-region-p)
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (buffer-string)))
         (sim-node-ids (mapcar #'car (ors--get-similar-nodes region-or-buffer)))
         (node (org-roam-node-read initial-input (lambda (node)
                                                   ;; if node's id is in the list of similar nodes, keep it
                                                   (member (org-roam-node-id node) sim-node-ids)) pred)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node other-window)
      (org-roam-capture-
       :node node
       :templates templates
       :props '(:finalize find-file)))))

;;;###autoload
(defun org-roam-similarity-section (node)
  "The similarity section for NODE."

  ;; NOTES:
  ;; - org-roam-node-marker shows how to find buffer visiting node file
  
  ;; get string text of current node
  ;; send that to the backend to get list of nodes
  (let* ((file (org-roam-node-file node))
         (id (org-roam-node-id node))
         (point (org-roam-node-point node))
         (buffer (or (find-buffer-visiting file)
                     (find-file-noselect file)))
         node-text
         node-ids)
    (with-current-buffer buffer
      (save-excursion
        (goto-char point)
        (setq node-text (if (or (string= "md" (file-name-extension file)) (= point 1))
                            (buffer-string)
                          (progn
                            (let* ((element (org-element-at-point))
                                   (begin (org-element-property :begin element))
                                   (end (org-element-property :end element)))
                              (buffer-substring begin end)) )
                          ))))


    ;; obviously the most similar node will be us, so we see ourselves out haha
    (when-let ((node-ids-scores (cl-remove-if (lambda (v) (string= (nth 0 v) id)) (ors--get-similar-nodes node-text))))
      (magit-insert-section (org-roam-similarity)
        (magit-insert-heading "Similar notes:")
        (dolist (node-id-score node-ids-scores)
          ;;
          (when-let ((snode (org-roam-node-from-id (car node-id-score))))
            (org-roam-node-insert-section
             :source-node snode
             :point (org-roam-node-point snode)
             ;; hack: I'm using the outline property to render the scores
             :properties `(:outline (,(format "%.3f" (nth 1 node-id-score)))))))
        (insert ?\n)))))

(provide 'org-roam-similarity)

