(require 'package)

(package-initialize)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package htmlize)
(use-package org-roam
  :init
  (setq org-roam-v2-ack t))
(use-package s)
(use-package ox-rss)

(load "~/.emacs.d/private/commonplace-lib/commonplace-lib.el")

(require 'ox-publish)
(require 'ox-html)
(require 'ox-rss)
(require 'htmlize)
(require 'org-roam)
(require 's)
(require 'find-lisp)
(require 'commonplace-lib)

;; Don't create backup files (those ending with ~) during the publish process.
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;
;; org-publish ;;
;;;;;;;;;;;;;;;;;

;; standard stuff here.
(setq commonplace/publish-url "https://commonplace.doubleloop.net")

(setq commonplace/preamble "
		<div class='flex flex-col sm:flex-row sm:items-center sm:justify-between'>
        <span class='flex flex-row sm:flex-row items-center sm:justify-between'>

                <a href='https://doubleloop.net/'><img src='/images/doubleloop.png' /></a>

                <nav id='site-navigation' class='main-navigation sm:pl-2 ml-5'>
                <div class='menu-main-container'><ul id='primary-menu' class='menu'>
<li id='menu-item-6884' class='menu-item menu-item-type-custom menu-item-object-custom menu-item-6884'><a href='https://doubleloop.net'>stream</a></li>
<li id='menu-item-6883' class='menu-item menu-item-type-custom menu-item-object-custom menu-item-6883'><a class='active' href='https://commonplace.doubleloop.net'>garden</a></li>
<li id='menu-item-7220' class='menu-item menu-item-type-post_type menu-item-object-page menu-item-7220'><a href='https://doubleloop.net/about/'>about</a></li>
</ul></div>                </nav><!-- #site-navigation -->
            </span>

            				    <p class='text-lg w-full hidden sm:block sm:w-1/2 sm:text-right ml-5 sm:ml-0 mr-1'>tech + politics + nature + culture</p>
					</div><!-- .site-branding -->
")
(setq commonplace/postamble "This page last updated: %C.  <a href='the-map.html'>Map</a>. <a href='recentchanges.html'>Recent changes</a>. <a href='https://gitlab.com/ngm/commonplace/'>Source</a>.  <a href='https://wiki.p2pfoundation.net/Peer_Production_License'>Peer Production License</a>.
<script async defer src='https://scripts.withcabin.com/hello.js'></script>
")
(setq commonplace/head-extra "
<link rel='me' href='mailto:neil@doubleloop.net' />
<link rel='webmention' href='https://webmention.io/commonplace.doubleloop.net/webmention' />
<link rel='pingback' href='https://webmention.io/commonplace.doubleloop.net/xmlrpc' />
<link href='https://fonts.googleapis.com/css?family=Nunito:400,700&display=swap' rel='stylesheet'>
<link href='https://unpkg.com/tippy.js@6.2.3/themes/light.css' rel='stylesheet'>
<link rel='stylesheet' type='text/css' href='/css/stylesheet.css'/>
<script src='/js/webmention.min.js'></script>
<script src='https://unpkg.com/@popperjs/core@2'></script>
<script src='https://unpkg.com/tippy.js@6'></script>
<script src='/js/URI.js'></script>
<script src='/js/page.js'></script>
")

(defun commonplace/filter-body (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (unless (org-export-derived-backend-p backend 'rss)
      (concat "<div class='e-content'>" text "</div>"))))

(add-to-list 'org-export-filter-body-functions
             'commonplace/filter-body)

; see https://vicarie.in/posts/blogging-with-org.html
(defun commonplace/sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]] @@html:</span>@@"
          (format-time-string "%d %h %Y"
                              (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

(defun commonplace/configure (project-dir publish-dir make-sitemap)
  (setq commonplace/project-dir project-dir)
  (commonplace/configure-org-publish project-dir publish-dir make-sitemap)

  ;; this is important - otherwise org-roam--org-roam-file-p doesnt work.
  (setq org-roam-directory project-dir)
  ;; to be able to find id links during publish
  (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))
  (setq org-roam-db-location (concat project-dir "/org-roam.db")))

(defun commonplace/configure-local ()
  (interactive)
  (commonplace/configure "/home/neil/commonplace" "/var/www/html/commonplace" nil)
  )

(defun silence (orig-func &rest args)
  (let ((inhibit-message t))
    (apply orig-func args)))

(defun commonplace/slugify-export-output-file-name-html (output-file)
  "Gets the title of the org file and uses this (slugified) for the output filename.
This is mainly to override org-roam's default filename convention of `timestamp-title_of_your_note`."
  (let* ((title (commonplace/get-title (buffer-file-name (buffer-base-buffer))))
         (directory (file-name-directory output-file))
         (slug (commonplace/slugify-title title)))
    (concat directory slug ".html")))

(defun commonplace/publish ()
  (commonplace/configure "/home/neil/commonplace" "/var/www/html/commonplace" nil)
  (advice-add 'org-export-output-file-name :filter-return #'commonplace/slugify-export-output-file-name-html)

  (rassq-delete-all 'html-mode auto-mode-alist)
  (rassq-delete-all 'web-mode auto-mode-alist)
  (fset 'web-mode (symbol-function 'fundamental-mode))
  (call-interactively 'org-publish-all))

;; republish all files, even if no changes made to the page content.
;; (for example, if you want backlinks to be regenerated).
(defun commonplace/republish ()
  (commonplace/configure "/home/neil/commonplace" "/var/www/html/commonplace" nil)
  (advice-add 'org-export-output-file-name :filter-return #'commonplace/slugify-export-output-file-name-html)

  ; current-prefix-arg is to force republish.
	(let ((current-prefix-arg 4))
    (rassq-delete-all 'web-mode auto-mode-alist)
    (fset 'web-mode (symbol-function 'fundamental-mode))
    (call-interactively 'org-publish-all)))

(defun org-publish-ignore-mode-hooks (orig-func &rest args)
  (let ((lexical-binding nil))
    (cl-letf (((symbol-function #'run-mode-hooks) #'ignore))
      (apply orig-func args))))


(defun commonplace/publish-remote ()
  (setq org-confirm-babel-evaluate nil)
  (setq org-publish-list-skipped-files nil)
  (commonplace/configure (file-truename ".") "../commonplace-html" nil)
  (org-roam-db-sync t)

  ; For output filename rewriting.
;  (advice-add 'org-export-output-file-name :filter-return #'commonplace/slugify-export-output-file-name-html)

  ; To try and speed things up.
;  (advice-add 'org-publish-all :around 'silence)
  (advice-add 'org-publish :around #'org-publish-ignore-mode-hooks)

  ; current-prefix-arg is to force republish.
  (let ((current-prefix-arg 4))
    ;(rassq-delete-all 'web-mode auto-mode-alist)
    ;(fset 'web-mode (symbol-function 'fundamental-mode))
    (call-interactively 'org-publish-all)))

(defun commonplace/publish-gitlab ()
  ;; (profiler-start 'cpu+mem)
  (setq org-confirm-babel-evaluate nil)
  (setq org-publish-list-skipped-files nil)
  (commonplace/configure (file-truename ".") "_posts" :makesitemap)
  (org-roam-db-sync t)

  ; For output filename rewriting.
;  (advice-add 'org-export-output-file-name :filter-return #'commonplace/slugify-export-output-file-name-html)

  ; To try and speed things up.
;  (advice-add 'org-publish-all :around 'silence)
  (advice-add 'org-publish :around #'org-publish-ignore-mode-hooks)

  ; current-prefix-arg is to force republish.
	(let ((current-prefix-arg 4))
    (rassq-delete-all 'web-mode auto-mode-alist)
    (fset 'web-mode (symbol-function 'fundamental-mode))
    (call-interactively 'org-publish-all))

  ;; (profiler-stop)
  ;; (profiler-report)
  ;; (profiler-report-write-profile "profile.txt")
  )

(defun commonplace/recent-changes-sitemap-function (title sitemap)
  (let* ((posts (cdr sitemap))
         (last-hundred (seq-subseq posts 0 (min (length posts) 100))))
    (org-list-to-org (cons (car sitemap) last-hundred))))

(defun commonplace/configure-org-publish (project-dir publish-dir make-sitemap)
  (let ((temp-dir (concat project-dir "/tempdir")))
    (setq org-publish-project-alist
          `(("commonplace"
             :components ("commonplace-notes" "commonplace-static" "commonplace-rss"))
            ("commonplace-notes"
             :base-directory ,project-dir
             :base-extension "org"
             :publishing-directory ,publish-dir
             :publishing-function org-html-publish-to-html
             :recursive t
             :headline-levels 4
             :with-toc nil
             :html-doctype "html5"
             :html-html5-fancy t
             :html-preamble ,commonplace/preamble
             :html-postamble ,commonplace/postamble
             :html-head-include-scripts nil
             :html-head-include-default-style nil
             :html-head-extra ,commonplace/head-extra
             :html-container "section"
             :htmlized-source nil
             :auto-sitemap make-sitemap
             :exclude "node_modules"
             :sitemap-title "Recent changes"
             :sitemap-sort-files anti-chronologically
             :sitemap-function commonplace/recent-changes-sitemap-function
             :sitemap-format-entry commonplace/sitemap-format-entry
             :sitemap-filename "recentchanges.org"
             )
            ("commonplace-rss"
             :base-directory ,temp-dir
             :base-extension "org"
             :publishing-directory ,publish-dir
             :publishing-function commonplace/publish-rss-feed
             :rss-extension "xml"
             :html-link-home ,commonplace/publish-url
             :html-link-use-abs-url t
             :html-link-org-files-as-html t
             :auto-sitemap t
             :sitemap-function commonplace/generate-org-for-rss-feed
             :sitemap-title "Recent activity in Neil's Digital Garden"
             :sitemap-filename "recentchanges-feed.org"
             :sitemap-style list
             :sitemap-sort-files anti-chronologically
             :sitemap-format-entry commonplace/format-rss-feed-entry)
            ("commonplace-static"
             :base-directory ,project-dir
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|svg\\|json\\|pdf"
             :publishing-directory ,publish-dir
             :exclude "node_modules"
             :recursive t
             :publishing-function org-publish-attachment)))))

(setq org-roam-title-to-slug-function 'commonplace/slugify-title)

;; org-roam backlinks
;; see https://org-roam.readthedocs.io/en/master/org_export/

(defun commonplace/collect-backlinks-string (backend)
  "Insert backlinks into the end of the org file before parsing it."
  (when (org-roam-node-at-point)
    (goto-char (point-max))
    ;; Add a new header for the references
    (insert "\nNotes that link to this note (AKA [[file:backlinks.org][backlinks]]).\n")
    (let* ((backlinks (org-roam-backlinks-get (org-roam-node-at-point))))
      (dolist (backlink backlinks)
        (let* ((source-node (org-roam-backlink-source-node backlink))
               (point (org-roam-backlink-point backlink)))
          (insert
           (format "- [[./%s][%s]]\n"
                   (file-name-nondirectory (org-roam-node-file source-node))
                   (org-roam-node-title source-node))))))))

(defun commonplace/link-to-agora (org-roam-node-at-point)
  (let* ((title (org-roam-node-title org-roam-node-at-point))
         (slug (commonplace/slugify-title title)))
    (concat "- [[https://anagora.org/" slug "][Anagora - " title "]] ")))

(defun commonplace/add-extra-sections (backend)
  (when (org-roam-node-at-point)
    (save-excursion
      (goto-char (point-max))
      (insert "\n* Elsewhere\n\n** In my garden")
      (commonplace/collect-backlinks-string backend)
      (insert "\n** In the Agora\n\n")
      (insert (commonplace/link-to-agora (org-roam-node-at-point)))
      (insert "\n** Mentions\n\n")
      (insert "#+BEGIN_EXPORT html
<div id='webmentions'></div>
#+END_EXPORT"))))

(add-hook 'org-export-before-processing-hook 'commonplace/add-extra-sections)

;; Fiddle with the HTML output.
;; TODO: note - a bad idea to override org-html-template!!
;; For now I couldn't figure out another way to hook into the HTML
;; to add the required markup for grid-container, grid, and page.
;; Came across this here: https://github.com/ereslibre/ereslibre.es/blob/b28ea388e2ec09b1033fc7eed2d30c69ba3ee827/config/default.el
;; Perhaps an alternative here?  https://vicarie.in/posts/blogging-with-org.html
(eval-after-load "ox-html"
  '(defun org-html-template (contents info)
     (concat (org-html-doctype info)
             "<html lang=\"en\">
                <head>"
             (org-html--build-meta-info info)
             (org-html--build-head info)
             (org-html--build-mathjax-config info)
             "</head>
                <body>"
             (org-html--build-pre/postamble 'preamble info)
             "<div class='grid-container'><div class='ds-grid'>"
             (unless (string= (org-export-data (plist-get info :title) info) "The Map")
               "<div class='page h-entry'>")
             ;; Document contents.
             (let ((div (assq 'content (plist-get info :html-divs))))
               (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
             ;; Document title.
             (when (plist-get info :with-title)
               (let ((title (and (plist-get info :with-title)
                                 (plist-get info :title)))
                     (subtitle (plist-get info :subtitle))
                     (html5-fancy (org-html--html5-fancy-p info)))
                 (when title
                   (format
                    (if html5-fancy
                        "<header>\n<h1 class=\"title p-name\">%s</h1> <a class='rooter' href='%s'>*</a>\n%s</header>"
                      "<h1 class=\"title p-name\">%s%s<a class='rooter' href='%s'>*</a></h1>\n")
                    (org-export-data title info)
                    (file-name-nondirectory (plist-get info :output-file))
                    (if subtitle
                        (format
                         (if html5-fancy
                             "<p class=\"subtitle\">%s</p>\n"
                           (concat "\n" (org-html-close-tag "br" nil info) "\n"
                                   "<span class=\"subtitle\">%s</span>\n"))
                         (org-export-data subtitle info))
                      "")))))
             ;; "<script type='text/javascript'>"
             ;; (with-temp-buffer
             ;;   (insert-file-contents "/home/shared/commonplace/graph.json")
             ;;   (buffer-string))
             ;; "</script>"
             (if (string= (org-export-data (plist-get info :title) info) "The Map")
                 (with-temp-buffer
                   ;(insert-file-contents (concat ,commonplace/project-dir "/graph.svg"))
                   (buffer-string)))
             contents
             (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
             "<div id='temp-network' style='display:none'></div>"
             "</div></div>"
             (unless (string= (org-export-data (plist-get info :title) info) "The Map")
               "</div>")
             (org-html--build-pre/postamble 'postamble info)
             "</body>
              </html>")))

;;;;;;;;;;;;;;;;;;;
;; Graph-related ;;
;;;;;;;;;;;;;;;;;;;

(defvar commonplace/graph-node-extra-config
        '(("shape"      . "rectangle")
          ("style"      . "rounded,filled")
          ("fillcolor"  . "#EEEEEE")
          ("fontname" . "sans")
          ("fontsize" . "10px")
          ("labelfontname" . "sans")
          ("color"      . "#C9C9C9")
          ("fontcolor"  . "#111111")))

;; Change the look of the graphviz graph a little.
(setq org-roam-graph-node-extra-config commonplace/graph-node-extra-config)

(defun commonplace/web-graph-builder (file)
  (concat (url-hexify-string (file-name-sans-extension (file-name-nondirectory file))) ".html"))

;; `org-roam-graph-node-url-builder` is not in master org-roam, I've added it to my local version.
;; see: https://github.com/ngm/org-roam/commit/82f40c122c836684a24a885f044dcc508212a17d
;; It's to allow setting a different URL for nodes on the graph.
(setq org-roam-graph-node-url-builder 'commonplace/web-graph-builder)

(setq org-roam-graph-exclude-matcher '("sitemap" "index" "recentchanges"))

;; Called from the Makefile.
;; It builds the graph and puts graph.dot and graph.svg in a place where I can publish them.
;; I exclude a few extra files from the graph here.
;; (I can't remember why I don't have them in the exclude-matcher!)
(defun commonplace/build-graph ()
  (let* ((node-query `[:select [titles:file titles:title tags:tags] :from titles
                               :left :join tags
                               :on (= titles:file tags:file)
                               :where :not (like title '"%2020%")
                               :and :not (like title '"%2019%")
                               :and :not (like title '"%All pages%")
                               :and :not (like title '"%Some books%")
                               :and :not (like title '"%Home%")])
         (graph      (org-roam-graph--dot node-query))
         (temp-dot (make-temp-file "graph." nil ".dot" graph))
         (temp-graph (make-temp-file "graph." nil ".svg")))
    (call-process "dot" nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (sit-for 5) ; TODO: switch to make-process (async) and callback to not need this.
    (copy-file temp-dot (concat commonplace/project-dir "/graph.dot") 't)
    (copy-file temp-graph (concat commonplace/project-dir "/graph.svg") 't)))


(defun commonplace/external-link-format (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match-p (regexp-quote "http") text)
      (s-replace "<a" "<a target='_blank' rel='noopener noreferrer'" text))))

(add-to-list 'org-export-filter-link-functions
             'commonplace/external-link-format)

(setq org-roam-server-network-label-wrap-length 20)
(setq org-roam-server-network-label-truncate t)
(setq org-roam-server-network-label-truncate-length 60)
(setq org-roam-server-extra-node-options nil)
(setq org-roam-server-extra-edge-options nil)
(setq org-roam-server-network-arrows nil)

(defun commonplace/recent-changes-sitemap-function (title sitemap)
  (let* ((posts (cdr sitemap))
         (last-hundred (seq-subseq posts 0 (min (length posts) 100))))
    (org-list-to-org (cons (car sitemap) last-hundred))))

; see https://writepermission.com/org-blogging-rss-feed.html
(defun commonplace/generate-org-for-rss-feed (title sitemap)
  "Generate a sitemap of posts that is exported as a RSS feed.
TITLE is the title of the RSS feed.  SITEMAP is an internal
representation for the files to include.  PROJECT is the current
project."
  (let* ((posts (cdr sitemap))
         (last-hundred (seq-subseq posts 0 (min (length posts) 100))))
    (concat "#+TITLE: " title "\n\n"
            (org-list-to-subtree (cons (car sitemap) last-hundred)))))

(defun commonplace/format-rss-feed-entry (entry _style project)
  "Format ENTRY for the posts RSS feed in PROJECT."
  (let* ((title (org-publish-find-title entry project))
         (link (concat (file-name-sans-extension entry) ".html"))
         (pubdate (format-time-string (car org-time-stamp-formats)
                                      (org-publish-find-date entry project))))
    (message pubdate)
    (format "%s
:properties:
:rss_permalink: %s
:pubdate: %s
:end:\n"
            title
            link
            pubdate)))

(defun commonplace/publish-rss-feed (plist filename dir)
  "Publish PLIST to RSS when FILENAME is rss.org.
DIR is the location of the output."
  (if (equal "recentchanges-feed.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename dir)))
