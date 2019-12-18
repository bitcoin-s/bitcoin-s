---
id: contributing-website
title: Contributing to the website
---

This website is built using [Docusaurus](https://docusaurus.io/).

For simple changes to the documentation, click on the `Edit` button at the top
of each page and submit those changes directly on GitHub.

## Scaladoc

One of the goals of Bitcoin-S is having useful and well-formatted Scaladoc comments on classes,
objects and functions. Here are some useful resources on how to properly format your Scaladoc comments:

- [Scaladoc for library authors](https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html)
- [Guidelines](https://docs.scala-lang.org/style/scaladoc.html) used by the official Scala language Scaladoc
- [Alvin Alexander guide](https://alvinalexander.com/scala/how-to-generate-scala-documentation-scaladoc-command-examples) on writing Scaladoc

To generate Scaladocs:

```bash
$ sbt
> unidoc
> docs/mdoc
```

This gets placed in `website/static/api`. When viewing the Docusaurus site the generated Scaladocs
appear under the API tab in the header bar,
or in the "API reference" link in the footer.

## Running the site locally

For running the website locally, you'll need:

- `yarn` (https://yarnpkg.com/lang/en/docs/install-ci/)
- `sbt` (https://www.scala-sbt.org/1.0/docs/Setup.html)

> In case you want to contribute substantial structural changes to the website,
> we suggest to read
> [Docusaurus' documentation](https://docusaurus.io/docs/en/installation.html)
> first.

You can now build and launch the website using
these commands:

```sh
cd website
yarn install # only the first time, to install the dependencies
yarn start
```

In a separate shell:

```bash
$ bloop run docs -- --watch
```

The above commands compiles our Mdoc Markdown files every time you change
them, and puts them in the correct directory for Docusaurus to find them.

Now visit http://localhost:3000/ and you should see a local version of
the website.

## Adding a new page

Whenever you add a new markdown page to the documentation, you'll have to
manually include it in the side menu.

You can do this by editing the `website/sidebars.json` file. The name to use is
the `id` specified in the page metadata (see the existing pages for an example).

## Publishing the site

```bash
$ sbt
> docs/publishWebsite
```

This command first generates Scaladocs, then invokes
`docs/docusaurusPublishGhPages`, which in turn compile our mdoc
files, build the site and push them to GH pages.

Before running those commands, you might have to change a few constants in
`siteConfig.js`. These are specifed in the comments of that file.

### CI

Bitcoin-S uses Travis to run tests and deploy library and website builds. Generally
speaking CI has to pass for a PR to get merged. If you make documentation/website only
changes, you can start your PR title with `Docs:`. This skips running tests on CI.
