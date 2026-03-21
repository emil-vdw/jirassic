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
 "jirassic-adf-parser.el"
 "jirassic-serializer.el")

(depends-on "emacs" "26.1")
(depends-on "aio" "1.0")
(depends-on "plz" "	0.9.1")
(depends-on "org" "9.5")

;;; Configure linters
(setq sentence-end-double-space nil)
