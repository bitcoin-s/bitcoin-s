/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");
const Markdown = CompLibrary.MarkdownBlock;

const Container = CompLibrary.Container;

class Users extends React.Component {
  render() {
    const { config: siteConfig } = this.props;
    if ((siteConfig.users || []).length === 0) {
      return null;
    }

    const editUrl = `${siteConfig.repoUrl}/edit/master/website/siteConfig.js`;
    const showcase = siteConfig.users.map(user => (
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          justifyContent: "space-between"
        }}
      >
        <a href={user.infoLink} key={user.infoLink}>
          <img src={user.image} alt={user.caption} title={user.caption} />
        </a>
        {user.description ? (
          <Container className="showcase-user-container">
            <Markdown>{user.description}</Markdown>
          </Container>
        ) : null}
      </div>
    ));

    return (
      <div className="mainContainer">
        <Container padding={["bottom", "top"]}>
          <div className="showcaseSection">
            <div className="prose">
              <h1>What is Bitcoin-S good for?</h1>
              <p>
                Bitcoin-S is a versatile and feature-rich Bitcoin framework that
                can power a vast array of Bitcoin and cryptocurrency
                applications. You could for example foo, bar or even baz, while
                leveraging qux and qaz.
              </p>
            </div>
            <h4>Here are some examples of companies using Bitcoin-S:</h4>
            <div className="logos">{showcase}</div>
            <p>Are you using this project?</p>
            <a href={editUrl} className="button">
              Add your company
            </a>
          </div>
        </Container>
      </div>
    );
  }
}

module.exports = Users;
