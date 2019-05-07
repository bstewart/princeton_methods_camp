This is the website for the Princeton Sociology Methods Camp for incoming Princeton PhD Students.

https://pusocmethodscamp.org/

Powered by [Jekyll](https://jekyllrb.com/) and [GitHub Pages](https://pages.github.com/).
# Table of contents

* [Editing](#editing)
  * [Local development environment](#local-development-environment)
  * [The default layout](#the-default-layout)
    * [Navigation](#navigation)
    * [Sections](#sections)

# Editing

## Local development environment

While not required, it is possible to setup a local development environment to
see the changes you make before they're live. The site is powered by Jekyll,
which written in Ruby. Installation steps:

1. Ensure you have `ruby` command installed
2. Run: `gem install bundler`
3. Clone this repository via git
4. Within the cloned git repository run: `bundle install`
5. Run: `bundle exec jekyll serve`

## The default layout

The default layout is used for the homepage of the site as indicated in
`index.md`. The default layout does the following things content-wise:

1. Output a navigation bar based on the `navigation` frontmatter in markdown
   file (e.g., `index.md`)
2. Output the content in the markdown file (e.g., `index.md`).
3. Output the content from any sections defined in the frontmatter.

### Navigation

The navigation is a list of links with a `name` key and a `url` key. The `url`
key can be any URL. However, in our usage we're including URLs to particular
sections in the document. These section links must match the id created
automatically for headers in the markdown files. For example, consider the
following markdown file:

```md
## Learning objectives

Learning objectives here.
```

When that markdown gets converted, the header will automatically receive an id
of `learning-objectives`. To link to this section, you'd use a `url` of
`#learning-objectives` in that navigation list. You may specify your own header
id with:

```md
## Learning objectives {#my-special-id}

Learning objectives here.
```

In this case, you should use `#my-special-id` for the `url` key when linking to
this section.


### Sections

You may add additional sections to the page by adding them to the `sections`
list in the markdown files frontmatter. When processing the sections, the
default layout will look for markdown files by that name and include their
content within each new section.
