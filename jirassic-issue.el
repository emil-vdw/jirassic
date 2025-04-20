;;; jirassic-issue.el --- Jira issue definitions -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:
(cl-defstruct jirassic-user
  "A Jira user."
  (account-id nil
              :read-only t
              :type string
              :documentation "The user ID, e.g. \"5b10ac8d4c0016482c7f3d0a\".")

  (link nil
        :read-only t
        :type string
        :documentation "Link to retrieve user account information.")

  (email-address nil
                 :read-only t
                 :type string
                 :documentation "User email address.")

  (display-name nil
                :read-only t
                :type string
                :documentation "User display name."))

(cl-defstruct jirassic-issue
  "A Jira issue."
  (id nil
      :read-only t
      :type string
      :documentation "The issue ID, e.g. \"1002\".")

  (key nil
       :read-only t
       :type string
       :documentation "The issue key, e.g. \"LPI-5041\".")

  (type nil
        :read-only t
        :type string
        :documentation "The issue type, e.g. \"Task\".")

  (priority nil
            :read-only t
            :type string
            :documentation "The issue priority, e.g. \"Major\".")

  (status nil
          :read-only t
          :type string
          :documentation "The issue status, e.g. \"In Progress\".")

  (creator nil
           :read-only t
           :type jirassic-user
           :documentation "The issue creator.")

  (project nil
           :read-only t
           :type string)

  (summary nil
           :read-only t
           :type string
           :documentation "The issue summary, e.g. \"Add support for flow rack roller type\".")

  (description nil
               :read-only t
               :type string
               :documentation "The issue description, e.g. \"Add support for flow rack roller type\".")

  (link nil
        :read-only t
        :type string
        :documentation "The issue link, e.g. \"https://jira.example.com/browse/LPI-5041\".")

  (attachments nil
               :read-only t
               :type vector
               :documentation "Vector of attachments for the issue."))


(provide 'jirassic-issue)

;;; jirassic-issue.el ends here
