/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");
const Markdown = CompLibrary.MarkdownBlock;
const Image = require(process.cwd() + "/core/Image.js");

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
        <Container padding={["bottom"]}>
          <div className="showcaseSection">
            <div className="prose">
              <h1>What is Bitcoin-S good for?</h1>
              <Image
                style={{ margin: "1em 0" }}
                src={siteConfig.baseUrl + "img/undraw_bitcoin2_ave7.svg"}
              />
              <p>
                Bitcoin-S is a versatile and feature-rich Bitcoin framework that
                can power a vast array of Bitcoin and cryptocurrency
                applications. Some examples of what Bitcoin-S is used for in
                production today:
              </p>
              <ul>
                <li>
                  Construct and sign transactions for withdrawals from a crypto
                  exchange
                </li>
                <li>
                  Spend to and from{" "}
                  <a href="https://en.bitcoin.it/wiki/Bech32">
                    Bech32 addresses
                  </a>
                  , enabling full SegWit support for your application
                </li>
                <li>
                  Parse{" "}
                  <a href="https://suredbits.com/lightning-101-what-is-a-lightning-invoice/">
                    Lightning Invoices
                  </a>{" "}
                  and other Lightning Network-native data structures
                </li>
                <li>
                  Interact with the{" "}
                  <a href="https://github.com/ACINQ/eclair">Eclair</a> Lightning
                  client, fast-tracking your application onto the Lightning
                  Network
                </li>
              </ul>
            </div>
            <h4>Here are some examples of companies using Bitcoin-S:</h4>
            <div className="logos">{showcase}</div>
            <p style={{ textAlign: "center" }}>Are you using this project?</p>
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
