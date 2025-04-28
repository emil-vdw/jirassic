;; -*- mode: eask; lexical-binding: t -*-

(package "jirassic"
         "0.1"
         "An Org centered Jira client")

(website-url "https://github.com/emil-vdw/jirassic")
(keywords "tools" "convenience" "jira")

(package-file "jirassic.el")

(load-paths "./" "./test/" "./test/fixtures/")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(files
 "jirassic.el"
 "jirassic-client.el"
 "jirassic-core.el"
 "jirassic-issue.el"
 "jirassic-org-roam.el"
 "jirassic-org.el"
 "jirassic-parser.el"
 "jirassic-serializer.el")

(depends-on "emacs" "26.1")
(depends-on "aio" "1.0.0")
(depends-on "f" "0.20.0")
(depends-on "dash" "2.0.0")
(depends-on "s" "1.12.0")
(depends-on "request" "0.3.0")
(depends-on "org" "9.5")
(depends-on "org-roam" "2.0")

;;; Configure linters
(setq sentence-end-double-space nil)
