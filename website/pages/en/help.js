/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");
const Image = require(process.cwd() + "/core/Image.js");

const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

function Help(props) {
  const { config: siteConfig, language = "" } = props;
  const { baseUrl, docsUrl } = siteConfig;
  const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
  const langPart = `${language ? `${language}/` : ""}`;
  const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

  const supportLinks = [
    {
      content: `Read the [guides and docs on this site.](${docUrl(
        "core/core-intro"
      )})`,
      title: "Browse Docs"
    },
    {
      content: `Explore the [Scaladocs](${
        siteConfig.scaladocUrl
      }) for the complete guide to how Bitcoin-S work`,
      title: "Browse API reference"
    },
    {
      content: [
        "If you have any questions about either the documentation or",
        "Bitcoin-S itself, the easiest way to get in touch with the",
        `developers is the \`#bitcoin-s\` channel in the [Suredbits Slack](${
          siteConfig.suredbitsSlack
        }).`,
        `There's also a [Gitter room](${
          siteConfig.gitterUrl
        }) you can join, although there's less`,
        "activity there."
      ].join(" "),
      title: "Join the community"
    }
  ];

  return (
    <div className="docMainWrapper wrapper">
      <Container className="mainContainer documentContainer postContainer">
        <div className="post">
          <header className="postHeader">
            <h1>Need help?</h1>
          </header>
          <Image src={baseUrl + "img/undraw_questions_75e0.svg"} />
          <GridBlock contents={supportLinks} layout="threeColumn" />
        </div>
      </Container>
    </div>
  );
}

module.exports = Help;
