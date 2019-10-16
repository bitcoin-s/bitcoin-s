/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

const Logo = props => (
  <div className="projectLogo">
    <img src={props.img_src} alt="Project Logo" />
  </div>
);

class HomeSplash extends React.Component {
  render() {
    const { siteConfig, language = "" } = this.props;
    const { baseUrl, docsUrl } = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
    const langPart = `${language ? `${language}/` : ""}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = props => (
      <div className="homeContainer">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">{props.children}</div>
        </div>
      </div>
    );

    const ProjectTitle = () => (
      <h2 className="projectTitle">
        <small>{siteConfig.tagline}</small>
      </h2>
    );

    const PromoSection = props => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    return (
      <SplashContainer>
        <div className="inner">
          <ProjectTitle siteConfig={siteConfig} />
          <PromoSection>
            {/*
            <Button href="#try">Try It Out</Button>
            <Button href={docUrl("doc1.html")}>Example Link</Button>
            <Button href={docUrl("doc2.html")}>Example Link 2</Button> */}
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

class Index extends React.Component {
  render() {
    const { config: siteConfig, language = "" } = this.props;
    const { baseUrl, docsUrl } = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
    const langPart = `${language ? `${language}/` : ""}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const Block = props => (
      <Container
        padding={["bottom", "top"]}
        id={props.id}
        background={props.background}
      >
        <GridBlock
          align="center"
          contents={props.children}
          layout={props.layout}
        />
      </Container>
    );

    const Button = props => (
      <div style={props.style} className="pluginWrapper buttonWrapper">
        <a className="dark-button" href={props.href} target={props.target}>
          {props.children}
        </a>
      </div>
    );

    // cribbed from Bloop site, https://github.com/scalacenter/bloop/blob/6b5384241d1bba4143315e66f668876d65a2e34f/website/pages/en/index.js#L92
    const Hero = ({ siteConfig }) => (
      <div className="hero">
        <div className="hero__container">
          <Logo img_src={`${siteConfig.baseUrl}img/bitcoin-s-logo.svg`} />
          <h1>{siteConfig.tagline}</h1>
          <Button href={docUrl("getting-started")}>Get started</Button>
        </div>
      </div>
    );

    const TryOut = () => (
      <Block id="try">
        {[
          {
            content: [
              "Use our RPC clients for `bitcoind`/Bitcoin Core and Eclair, and get powerful",
              "static typing baked into your RPC calls. All returned values you get from `bitcoind`",
              "and Eclair are converted into native Bitcoin/Lightning data structures for you.",
              "Is that raw hex string you've been passing around a transaction or a Lightning invoice?",
              "With Bitcoin-S you get both confidence in your code _and_ powerful methods available",
              "on your data"
            ].join(" "),
            image: `${baseUrl}img/undraw_target_kriv.svg`,
            imageAlign: "left",
            title: "Super-powered RPC clients"
          }
        ]}
      </Block>
    );

    const Description = () => (
      <Block background="dark">
        {[
          {
            content: [
              "Bitcoin-S is used in production today, facilitating transactions and systems handling",
              "millions of dollars each day. It has a very high degree of code coverage,",
              "with unit tests, property based tests and integration tests."
            ].join(" "),
            image: `${baseUrl}img/undraw_Security_on_s9ym.svg`,
            imageAlign: "right",
            title: "Battle tested, high quality code"
          }
        ]}
      </Block>
    );

    const LearnHow = () => (
      <Block background="light">
        {[
          {
            content: [
              "We provide solid APIs for constructing and signing transactions.",
              "From small-scale 1-in 2-out transactions, to custom logic powering exchange withdrawals, we've got you covered.",
              "Check out our [`TxBuilder` example](docs/core/txbuilder) to see how."
            ].join(" "),
            image: `${baseUrl}img/undraw_transfer_money_rywa.svg`,
            imageAlign: "right",
            title: "Construct and sign bitcoin transactions"
          }
        ]}
      </Block>
    );

    const Features = () => (
      <Block layout="fourColumn">
        {[
          {
            content: [
              "Bitcoin-S allows you to interact with data structures found in the",
              "Bitcoin and Lightning protocols as first-class citizens of your app.",
              "Go back and forth between hex, byte and JVM representation trivially,",
              "letting our abstractions focus on what you want to build"
            ].join(" "),
            image: `${baseUrl}img/undraw_digital_currency_qpak.svg`,
            imageAlign: "top",
            title: "Deep protocol understanding"
          },
          {
            content: [
              "Code with confidence, knowing your data won't change under you. All",
              "data structures in Bitcoin-S are immutable. This eliminates a big",
              "range of bugs right away, and enable you to write concurrent code",
              "much easier"
            ].join(" "),
            image: `${baseUrl}img/undraw_code_review_l1q9.svg`,
            imageAlign: "top",
            title: "Immutable data structures"
          },
          {
            content: [
              "Get the compiler to work for you, ensuring your logic covers all cases.",
              "Modelling your application with mathematically founded types enables greater confidence in the correctness of your code"
            ].join(" "),
            image: `${baseUrl}img/undraw_mathematics_4otb.svg`,
            imageAlign: "top",
            title: "Algebraic data types"
          }
        ]}
      </Block>
    );

    const Showcase = () => {
      if ((siteConfig.users || []).length === 0) {
        return null;
      }

      const showcase = siteConfig.users
        .filter(user => user.pinned)
        .map(user => (
          <a href={user.infoLink} key={user.infoLink}>
            <img src={user.image} alt={user.caption} title={user.caption} />
          </a>
        ));

      const pageUrl = page => baseUrl + (language ? `${language}/` : "") + page;

      return (
        <div className="productShowcaseSection paddingBottom">
          <h2>Who is using Bitcoin-S?</h2>
          <p>
            Bitcoin-S is used in production applications, by both small and
            large companies
          </p>
          <div className="logos">{showcase}</div>
          <div className="more-users">
            <a className="button" href={pageUrl("users.html")}>
              Read more
            </a>
          </div>
        </div>
      );
    };

    return (
      <div>
        <Hero siteConfig={siteConfig} />
        <div className="mainContainer">
          <Features />
          <LearnHow />
          <TryOut />
          <Description />
          <Showcase />
        </div>
      </div>
    );
  }
}

module.exports = Index;
