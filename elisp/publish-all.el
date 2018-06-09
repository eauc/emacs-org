(defun publish-all (title base-dir publishing-dir)
  (progn
    (setq org-publish-project-alist
          `(("doc-html"
             :base-directory ,base-dir
             ;; :base-extension "org"
             :publishing-directory ,publishing-dir
             :publishing-function org-html-publish-to-html
             ;; :html-extension "html"
             :section-numbers nil
             :recursive t
             :with-toc t
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title ,title
             :sitemap-function org-publish-org-sitemap
             :sitemap-format-entry org-publish-org-sitemap-format
             ;; :table-of-contents nil
             ;; :headline-levels 6
             ;; :body-only t
             )))
    (setq org-publish-use-timestamps-flag nil
          org-src-fontify-natively t
          org-html-htmlize-output-type 'css)
    (org-publish-all)))
(defun org-publish-org-sitemap (title list)
  (concat (format "#+TITLE: %s\n#+SETUPFILE: ./publish.setup\n\n" title)
          (org-list-to-subtree list)))
(defun org-publish-org-sitemap-format (entry style project)
  (cond ((not (directory-name-p entry))
         (format "[[file:./%s][%s]]"
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))
