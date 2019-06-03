/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

/**
 * For github.io type URLs, you would set the url and baseUrl like:
 *  const url = 'https://your-github-user-name.github.io'
 *  const baseUrl = "/bitcoin-s/"
 *
 * This would place the site under
 * https://your-user-name.github.io/bitcoin-s.
 * If publishing under a custom domain with no path
 * after the domain, you'd set const baseUrl = "/"
 * and const url = "https://your-domain.org".
 */

const url = "https://bitcoin-s.org";
const baseUrl = "/";
const scaladocUrl = baseUrl + "api/org/bitcoins";

/**
 * This should be the username/organization that owns the repo
 * you're publishing the website for.
 */
const organizationName = "bitcoin-s";

/**
 * This should be the name of the repo you're publishing the
 * website for
 */
const projectName = "bitcoin-s";

// List of projects/orgs using your project for the users page.
const users = [
  /*
  This is how a user description should look. The description field is optional.
  You can use markdown in your company description.
  {
    caption: "The name of your company",
    image: "/img/your-company-logo.png",
    description: "Describe how your company uses bitcoin-s",
    pinned: true
  },
  */
  {
    caption: "Suredbits",
    image: `${baseUrl}img/suredbits-logo.png`,
    infoLink: "https://suredbits.com",
    description: "Suredbits uses Bitcoin-S to power their Lightning APIs.",
    pinned: true
  },
  {
    caption: "Gemini",
    image: `${baseUrl}img/gemini-logo.png`,
    infoLink: "https://gemini.com",
    description: [
      "Gemini uses Bitcoin-S to batch transactions and facilitate deposits and",
      "withdrawals with full SegWit support.",
      "Read more at [their blog](https://medium.com/gemini/gemini-upgrades-wallet-with-full-support-of-segwit-5bb8e4bc851b)"
    ].join(" "),
    pinned: true
  }
];

const siteConfig = {
  title: "bitcoin-s", // Title for your website.
  tagline: "Bitcoin implementation in Scala",
  url,
  baseUrl,

  // URL for editing docs, has to be present for the
  // "Edit this Doc" button to appear
  editUrl: "https://github.com/bitcoin-s/bitcoin-s/docs",

  // Used for publishing and more
  projectName,
  organizationName,

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    { doc: "core/core-intro", label: "Docs" },
    { href: scaladocUrl, label: "API" },
    { page: "help", label: "Help" },
    { blog: true, label: "Blog" }
  ],

  /** Makes the user showcase appear */
  users,

  /* path to images for header/footer */
  headerIcon: "img/favicon.ico",
  footerIcon: "img/favicon.ico",
  favicon: "img/favicon.ico",

  /* Colors for website */
  colors: {
    primaryColor: "#1f7a8c", // teal
    secondaryColor: "#bfdbf7" // light-ish blue
  },

  /* Custom fonts for website */
  fonts: {
    headerFont: ["Montserrat", "sans-serif"]
  },

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Suredbits & the bitcoin-s developers`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: "default"
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: [
    "https://buttons.github.io/buttons.js",
    "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js",
    "https://fonts.googleapis.com/css?family=Montserrat:500",
    `${baseUrl}js/code-block-buttons.js`
  ],

  stylesheets: [`${baseUrl}css/code-block-buttons.css`],

  // On page navigation for the current documentation page.
  onPageNav: "separate",
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: "img/undraw_online.svg",
  twitterImage: "img/undraw_tweetstorm.svg",

  // Show documentation's last contributor's name.
  enableUpdateBy: true,

  // Show documentation's last update time.
  enableUpdateTime: true,

  // don't use Docusarus CSS for Scaladocs,
  // and don't let Scaladoc CSS influence
  // Docusaurus
  separateCss: ["api"],

  // mdoc writes docs to this directory
  customDocsPath: "bitcoin-s-docs/target/mdoc",

  ////////////////////
  // custom keys begin
  repoUrl: "https://github.com/bitcoin-s/bitcoin-s",
  suredbitsSlack:
    "https://join.slack.com/t/suredbits/shared_invite/enQtNDEyMjY3MTg1MTg3LTYyYjkwOGUzMDQ4NDAwZjE1M2I3MmQyNWNlZjNlYjg4OGRjYTRjNWUwNjRjNjg4Y2NjZjAxYjU1N2JjMTU1YWM",
  gitterUrl: "https://gitter.im/bitcoin-s-core/",
  // avoid showing "root" as default Scaladoc page
  scaladocUrl

  // custom keys end
  //////////////////
};

module.exports = siteConfig;
