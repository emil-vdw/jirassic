(defvar jirassic-complex-issue-data
  '((expand
     . "renderedFields,names,schema,operations,editmeta,changelog,versionedRepresentations")
    (id . "10000")
    (self . "https://jirassic.atlassian.net/rest/api/3/issue/10000")
    (key . "KAN-1")
    (fields (statuscategorychangedate . "2025-04-26T11:45:03.883+0200")
            (issuetype
             (self
              . "https://jirassic.atlassian.net/rest/api/3/issuetype/10001")
             (id . "10001")
             (description . "A small, distinct piece of work.")
             (iconUrl
              . "https://jirassic.atlassian.net/rest/api/2/universal_avatar/view/type/issuetype/avatar/10318?size=medium")
             (name . "Task") (subtask . :json-false) (avatarId . 10318)
             (entityId . "c2c79080-91f5-47b4-9aa7-c97fc6b0dffb")
             (hierarchyLevel . 0))
            (components . []) (timespent) (timeoriginalestimate)
            (project
             (self
              . "https://jirassic.atlassian.net/rest/api/3/project/10000")
             (id . "10000") (key . "KAN") (name . "Jirassic")
             (projectTypeKey . "software") (simplified . t)
             (avatarUrls
              (48x48
               . "https://jirassic.atlassian.net/rest/api/3/universal_avatar/view/type/project/avatar/10422")
              (24x24
               . "https://jirassic.atlassian.net/rest/api/3/universal_avatar/view/type/project/avatar/10422?size=small")
              (16x16
               . "https://jirassic.atlassian.net/rest/api/3/universal_avatar/view/type/project/avatar/10422?size=xsmall")
              (32x32
               . "https://jirassic.atlassian.net/rest/api/3/universal_avatar/view/type/project/avatar/10422?size=medium")))
            (description (type . "doc") (version . 1)
                         (content
                          . [((type . "heading") (attrs (level . 1))
                              (content
                               . [((type . "text")
                                   (text
                                    . "🛠️ Test Issue for API Serialization"))]))
                             ((type . "paragraph")
                              (content
                               . [((type . "text")
                                   (text
                                    . "This is a dummy Jira ticket created for testing purposes.")
                                   (marks . [((type . "em"))]))
                                  ((type . "hardBreak"))
                                  ((type . "text")
                                   (text
                                    . "The goal is to validate the serialization and deserialization of Jira API responses within our application."))]))
                             ((type . "rule"))
                             ((type . "heading") (attrs (level . 2))
                              (content
                               . [((type . "text")
                                   (text . "📋 Acceptance Criteria"))]))
                             ((type . "bulletList")
                              (content
                               . [((type . "listItem")
                                   (content
                                    . [((type . "paragraph")
                                        (content
                                         . [((type . "text")
                                             (text
                                              . "The issue can be retrieved via the API."))]))]))
                                  ((type . "listItem")
                                   (content
                                    . [((type . "paragraph")
                                        (content
                                         . [((type . "text")
                                             (text
                                              . "The serialized fields match the original Jira fields (e.g., "))
                                            ((type . "text")
                                             (text . "summary")
                                             (marks . [((type . "code"))]))
                                            ((type . "text")
                                             (text . ", "))
                                            ((type . "text")
                                             (text . "description")
                                             (marks . [((type . "code"))]))
                                            ((type . "text")
                                             (text . ", "))
                                            ((type . "text")
                                             (text . "status")
                                             (marks . [((type . "code"))]))
                                            ((type . "text")
                                             (text . ", "))
                                            ((type . "text")
                                             (text . "priority")
                                             (marks . [((type . "code"))]))
                                            ((type . "text")
                                             (text . ", "))
                                            ((type . "text")
                                             (text . "assignee")
                                             (marks . [((type . "code"))]))
                                            ((type . "text")
                                             (text . ")."))]))]))
                                  ((type . "listItem")
                                   (content
                                    . [((type . "paragraph")
                                        (content
                                         . [((type . "text")
                                             (text
                                              . "No unexpected fields or errors occur during parsing."))]))]))]))
                             ((type . "paragraph"))
                             ((type . "codeBlock")
                              (attrs (language . "c"))
                              (content
                               . [((type . "text")
                                   (text
                                    . "int main() {\n  printf(\"Hello World!\");\n  return 0;\n} "))]))
                             ((type . "rule"))
                             ((type . "heading") (attrs (level . 2))
                              (content
                               . [((type . "text")
                                   (text . "🧪 Steps to Reproduce"))]))
                             ((type . "heading") (attrs (level . 3))
                              (content
                               . [((type . "text")
                                   (text
                                    . "Create a request to fetch this issue via the Jira API:"))]))
                             ((type . "paragraph"))
                             ((type . "expand")
                              (content
                               . [((type . "paragraph")
                                   (content
                                    . [((type . "text")
                                        (text
                                         . "GET /rest/api/3/issue/PROJ-123"))]))])
                              (attrs (title . "")))
                             ((type . "expand")
                              (content
                               . [((type . "paragraph")
                                   (content
                                    . [((type . "text")
                                        (text
                                         . "GET /rest/api/3/issue/PROJ-123"))]))])
                              (attrs (title . "Old API")))
                             ((type . "heading") (attrs (level . 3))
                              (content
                               . [((type . "text")
                                   (text . "Serialize the "))
                                  ((type . "text") (text . "response")
                                   (marks
                                    . [((type . "subsup")
                                        (attrs (type . "sub")))]))
                                  ((type . "text")
                                   (text
                                    . " into your application's model."))]))
                             ((type . "heading") (attrs (level . 3))
                              (content
                               . [((type . "text")
                                   (text
                                    . "Validate the model fields against expected values."))]))
                             ((type . "rule"))
                             ((type . "heading") (attrs (level . 2))
                              (content
                               . [((type . "text") (text . "📌 Priority"))]))
                             ((type . "paragraph")
                              (content
                               . [((type . "text") (text . "Medium")
                                   (marks . [((type . "em"))]))]))
                             ((type . "heading") (attrs (level . 2))
                              (content
                               . [((type . "text") (text . "🏷️ Labels"))]))
                             ((type . "bulletList")
                              (content
                               . [((type . "listItem")
                                   (content
                                    . [((type . "paragraph")
                                        (content
                                         . [((type . "text")
                                             (text . "testing")
                                             (marks . [((type . "code"))]))]))]))
                                  ((type . "listItem")
                                   (content
                                    . [((type . "paragraph")
                                        (content
                                         . [((type . "text")
                                             (text . "api")
                                             (marks . [((type . "code"))]))]))]))
                                  ((type . "listItem")
                                   (content
                                    . [((type . "paragraph")
                                        (content
                                         . [((type . "text")
                                             (text . "serialization")
                                             (marks . [((type . "code"))]))]))]))]))
                             ((type . "rule"))
                             ((type . "heading") (attrs (level . 2))
                              (content
                               . [((type . "text")
                                   (text . "⚙️ Additional Info"))]))
                             ((type . "paragraph")
                              (content
                               . [((type . "text") (text . "bq. "))
                                  ((type . "text")
                                   (text
                                    . "This issue is only for internal testing. No action needed from the product team.")
                                   (marks . [((type . "em"))]))]))
                             ((type . "paragraph"))
                             ((type . "panel")
                              (content
                               . [((type . "paragraph")
                                   (content
                                    . [((type . "text")
                                        (text
                                         . "Include some useless information "))
                                       ((type . "status")
                                        (attrs (text . "Done")
                                               (color . "green")
                                               (localId
                                                . "4f3c283d-b906-41dd-ab44-d4cdfa9960a1")
                                               (style . "")))]))
                                  ((type . "paragraph")
                                   (content
                                    . [((type . "text")
                                        (text . " second line"))]))])
                              (attrs (panelType . "info")))
                             ((type . "table")
                              (attrs
                               (isNumberColumnEnabled . :json-false)
                               (layout . "align-start")
                               (localId
                                . "c70ed161-f435-4357-b8c8-d7ef068afc18"))
                              (content
                               . [((type . "tableRow")
                                   (content
                                    . [((type . "tableHeader") (attrs)
                                        (content
                                         . [((type . "paragraph"))]))
                                       ((type . "tableHeader") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "number")
                                                  (marks
                                                   . [((type . "strong"))]))]))]))
                                       ((type . "tableHeader") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "square")
                                                  (marks
                                                   . [((type . "strong"))]))]))]))]))
                                  ((type . "tableRow")
                                   (content
                                    . [((type . "tableCell") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "even"))]))]))
                                       ((type . "tableCell") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "5"))]))]))
                                       ((type . "tableCell") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "25"))]))]))]))
                                  ((type . "tableRow")
                                   (content
                                    . [((type . "tableCell") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "odd"))]))]))
                                       ((type . "tableCell") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "2"))]))]))
                                       ((type . "tableCell") (attrs)
                                        (content
                                         . [((type . "paragraph")
                                             (content
                                              . [((type . "text")
                                                  (text . "4"))]))]))]))]))]))
            (fixVersions . []) (aggregatetimespent)
            (statusCategory
             (self
              . "https://jirassic.atlassian.net/rest/api/3/statuscategory/4")
             (id . 4) (key . "indeterminate") (colorName . "yellow")
             (name . "In Progress"))
            (customfield_10035) (resolution) (timetracking)
            (customfield_10015) (security)
            (attachment
             . [((self
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/10000")
                 (id . "10000") (filename . ".Xresources")
                 (author
                  (self
                   . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
                  (accountId
                   . "91128ee2-a476-4306-909b-af3cb0ca7ec9")
                  (emailAddress . "bob@gmail.com")
                  (avatarUrls
                   (48x48
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (24x24
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (16x16
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (32x32
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
                  (displayName . "Pieter Koekemoer") (active . t)
                  (timeZone . "Europe/Brussels")
                  (accountType . "atlassian"))
                 (created . "2025-04-26T12:51:30.294+0200") (size . 30)
                 (mimeType . "text/plain")
                 (content
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/content/10000"))
                ((self
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/10001")
                 (id . "10001")
                 (filename
                  . "DALL·E 2024-12-06 21.22.00 - A whimsical, artistic representation of a shared photo space, inspired by celestial and magical themes. The image features a glowing camera lens float.webp")
                 (author
                  (self
                   . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
                  (accountId
                   . "712020:1efec56e-9131-488c-8d3c-6be26314e421")
                  (emailAddress . "pkoekemoer@gmail.com")
                  (avatarUrls
                   (48x48
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (24x24
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (16x16
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (32x32
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
                  (displayName . "Pieter Koekemoer") (active . t)
                  (timeZone . "Europe/Brussels")
                  (accountType . "atlassian"))
                 (created . "2025-04-26T15:25:15.988+0200")
                 (size . 464962) (mimeType . "image/webp")
                 (content
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/content/10001"))
                ((self
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/10003")
                 (id . "10003") (filename . "foo.org")
                 (author
                  (self
                   . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
                  (accountId
                   . "712020:1efec56e-9131-488c-8d3c-6be26314e421")
                  (emailAddress . "pkoekemoer@gmail.com")
                  (avatarUrls
                   (48x48
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (24x24
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (16x16
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (32x32
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
                  (displayName . "Pieter Koekemoer") (active . t)
                  (timeZone . "Europe/Brussels")
                  (accountType . "atlassian"))
                 (created . "2025-04-26T15:26:12.422+0200") (size . 71)
                 (mimeType . "text/plain")
                 (content
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/content/10003"))
                ((self
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/10002")
                 (id . "10002") (filename . "foo.py")
                 (author
                  (self
                   . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
                  (accountId
                   . "712020:1efec56e-9131-488c-8d3c-6be26314e421")
                  (emailAddress . "pkoekemoer@gmail.com")
                  (avatarUrls
                   (48x48
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (24x24
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (16x16
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
                   (32x32
                    . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
                  (displayName . "Pieter Koekemoer") (active . t)
                  (timeZone . "Europe/Brussels")
                  (accountType . "atlassian"))
                 (created . "2025-04-26T15:26:12.334+0200") (size . 1156)
                 (mimeType . "text/plain")
                 (content
                  . "https://jirassic.atlassian.net/rest/api/3/attachment/content/10002"))])
            (aggregatetimeestimate) (resolutiondate) (workratio . -1)
            (summary . "Test Issue for API Serialization")
            (issuerestriction (issuerestrictions) (shouldDisplay . t))
            (watches
             (self
              . "https://jirassic.atlassian.net/rest/api/3/issue/KAN-1/watchers")
             (watchCount . 1) (isWatching . t))
            (lastViewed . "2025-04-26T11:40:34.244+0200")
            (creator
             (self
              . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
             (accountId . "712020:1efec56e-9131-488c-8d3c-6be26314e421")
             (emailAddress . "pkoekemoer@gmail.com")
             (avatarUrls
              (48x48
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (24x24
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (16x16
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (32x32
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
             (displayName . "Pieter Koekemoer") (active . t)
             (timeZone . "Europe/Brussels") (accountType . "atlassian"))
            (subtasks . []) (created . "2025-04-26T11:40:26.872+0200")
            (customfield_10021)
            (reporter
             (self
              . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
             (accountId . "712020:1efec56e-9131-488c-8d3c-6be26314e421")
             (emailAddress . "pkoekemoer@gmail.com")
             (avatarUrls
              (48x48
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (24x24
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (16x16
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (32x32
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
             (displayName . "Pieter Koekemoer") (active . t)
             (timeZone . "Europe/Brussels") (accountType . "atlassian"))
            (aggregateprogress (progress . 0) (total . 0))
            (priority
             (self
              . "https://jirassic.atlassian.net/rest/api/3/priority/3")
             (iconUrl
              . "https://jirassic.atlassian.net/images/icons/priorities/medium_new.svg")
             (name . "Medium") (id . "3"))
            (customfield_10001) (labels . []) (environment)
            (customfield_10019 . "0|hzzzzz:") (timeestimate)
            (aggregatetimeoriginalestimate) (versions . []) (duedate)
            (progress (progress . 0) (total . 0)) (issuelinks . [])
            (votes
             (self
              . "https://jirassic.atlassian.net/rest/api/3/issue/KAN-1/votes")
             (votes . 0) (hasVoted . :json-false))
            (comment (comments . [])
                     (self
                      . "https://jirassic.atlassian.net/rest/api/3/issue/10000/comment")
                     (maxResults . 0) (total . 0) (startAt . 0))
            (assignee
             (self
              . "https://jirassic.atlassian.net/rest/api/3/user?accountId=712020%3A1efec56e-9131-488c-8d3c-6be26314e421")
             (accountId . "712020:1efec56e-9131-488c-8d3c-6be26314e421")
             (emailAddress . "pkoekemoer@gmail.com")
             (avatarUrls
              (48x48
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (24x24
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (16x16
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png")
              (32x32
               . "https://secure.gravatar.com/avatar/add36ea662812008ebb1edc0ed75ec1e?d=https%3A%2F%2Favatar-management--avatars.us-west-2.prod.public.atl-paas.net%2Finitials%2FEW-1.png"))
             (displayName . "Pieter Koekemoer") (active . t)
             (timeZone . "Europe/Brussels") (accountType . "atlassian"))
            (worklog (startAt . 0) (maxResults . 20) (total . 0)
                     (worklogs . []))
            (updated . "2025-04-27T15:30:42.130+0200")
            (status
             (self
              . "https://jirassic.atlassian.net/rest/api/3/status/10001")
             (description
              . "This work item is being actively worked on at the moment by the assignee.")
             (iconUrl . "https://jirassic.atlassian.net/")
             (name . "In Progress") (id . "10001")
             (statusCategory
              (self
               . "https://jirassic.atlassian.net/rest/api/3/statuscategory/4")
              (id . 4) (key . "indeterminate") (colorName . "yellow")
              (name . "In Progress"))))))

(provide 'jirassic-complex-issue-data)
