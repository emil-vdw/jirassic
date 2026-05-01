;; -*- mode: eask; lexical-binding: t -*-

(package "jirassic"
         "0.1"
         "An Org centered Jira client")

(website-url "https://github.com/emil-vdw/jirassic")
(keywords "tools" "convenience" "jira")

(package-file "jirassic.el")

(load-paths "./" "./test/" "./test/fixtures/")

(source "gnu")
(source "melpa")

(files
 "jirassic.el"
 "jirassic-client.el"
 "jirassic-jira.el"
 "jirassic-jira-parser.el"
 "jirassic-org-serializer.el"
 "jirassic-org.el"
 "jirassic-org-roam.el")

(depends-on "emacs" "26.1")
(depends-on "aio" "1.0")
(depends-on "plz" "	0.9.1")
(depends-on "org" "9.5")
(depends-on "seq" "2.24")

;;; Configure linters
(setq sentence-end-double-space nil)
