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

Now visit http://localhost:3000/ and you should see a local version of
the website.

## Adding a new page

Whenever you add a new markdown page to the documentation, you'll have to
manually include it in the side menu.

You can do this by editing the `website/sidebars.json` file. The name to use is
the `id` specified in the page metadata (see the existing pages for an example).
