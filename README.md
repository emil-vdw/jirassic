# Jirassic

Jirassic is an Org-centered Jira client for Emacs. It fetches Jira issues and renders them as native Org markup without leaving Emacs — for Org-mode users with Jira-based workflows.

## Features

- **Jira REST API client**
  Async HTTP via `aio` + `plz`, secure credential storage via `auth-source`
- **ADF → Org-mode conversion**
  Converts Atlassian Document Format rich text to Org markup (25+ node types: headings, lists, code blocks, tables, panels, blockquotes, etc.)
- **Org capture integration**
  Org capture support via `jirassic-org-capture` with a rich template variable system (see TODO)
- **Org-roam integration**
  Org-roam capture support via `jirassic-org-roam-capture` with extra capture context from the issue (see TODO)

## Capture Template Variables
