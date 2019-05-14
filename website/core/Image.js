const React = require("react");

const Image = ({ src, style }) => (
  <img
    style={{
      maxWidth: "50%",
      // center the image
      margin: "0 25%",
      ...style
    }}
    src={src}
  />
);

module.exports = Image;
