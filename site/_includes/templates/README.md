By convention, templates are keps in here.

Templates can use other templates by including the following frontmatter:

```
---
layout: templates/template_name.njk # or ./template_name.njk, if you don't want to use relative
---
```