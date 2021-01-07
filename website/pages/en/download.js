const React = require("react");

const CompLibrary = require("../../core/CompLibrary");

const Container = CompLibrary.Container;

const CWD = process.cwd();

const versions = require(`${CWD}/versions.json`);

function Downloads(props) {
    const {config: siteConfig} = props;
    const latestVersion = versions[0];
    const releaseUrl = `https://github.com/${siteConfig.organizationName}/${siteConfig.projectName}/releases`;

    return (
        <div className="docMainWrapper wrapper">
            <Container className="mainContainer versionsContainer">
                <div className="post">
                    <header className="postHeader">
                        <h1>{siteConfig.title} downloads</h1>
                    </header>
                    <p>New versions of this project are released every so often.</p>
                    <h3 id="latest">Current version</h3>
                    <table className="versions">
                        <tbody>
                        <tr>
                            <th>{latestVersion}</th>
                            <td>
                                <a href={`${releaseUrl}/tag/v${latestVersion}`}>
                                    Release
                                </a>
                            </td>
                            <td>
                                <a href={siteConfig.scaladocUrl}>Scaladoc</a>
                            </td>
                        </tr>
                        </tbody>
                    </table>
                    <table>
                        <tbody>
                        <tr>
                            <td>
                                <a href={`${releaseUrl}/download/v${latestVersion}/bitcoin-s-server-${latestVersion}.dmg`}>bitcoin-s-server-{latestVersion}.dmg</a>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <a href={`${releaseUrl}/download/v${latestVersion}/bitcoin-s-server-${latestVersion}.tgz`}>bitcoin-s-server-{latestVersion}.tgz</a>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <a href={`${releaseUrl}/download/v${latestVersion}/bitcoin-s-server-docker.zip`}>bitcoin-s-server-docker.zip</a>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <a href={`${releaseUrl}/download/v${latestVersion}/bitcoin-s-server.deb`}>bitcoin-s-server.deb</a>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <a href={`${releaseUrl}/download/v${latestVersion}/bitcoin-s-server.msi`}>bitcoin-s-server.msi</a>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <a href={`${releaseUrl}/download/v${latestVersion}/SHA256SUMS.asc`}>SHA256SUMS.asc</a>
                            </td>
                        </tr>
                        </tbody>
                    </table>
                </div>
            </Container>
        </div>
    );
}

module.exports = Downloads;
