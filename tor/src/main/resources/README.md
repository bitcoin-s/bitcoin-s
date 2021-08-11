# Updating Executables

Check if there is a new Tor Browser version.

The Tor Browser changelog can be found here: https://gitweb.torproject.org/builders/tor-browser-build.git/plain/projects/tor-browser/Bundle-Data/Docs/ChangeLog.txt

The Tor changelog can be found here: https://gitweb.torproject.org/tor.git/plain/ChangeLog

Download the latest stable Tor Browser from here: https://www.torproject.org/download/

- Windows x64
- Linux x64
- macOS x64

Do not copy the PluggableTransports folder!

## Windows
msi => Browser\TorBrowser\Tor

## Linux
tar.gz => tor-browser_en-US\Browser\TorBrowser\Tor

## macOS
dmg => Tor Browser.app\Contents\MacOS\Tor

Do not delete the Tor file from the original folder!

1. Replace executables.
2. Make sure the binaries are executable:
	`git update-index --chmod=+x tor/natives/linux_64/tor`
	`git update-index --chmod=+x tor/natives/windows_64/tor.exe`
	`git update-index --chmod=+x tor/natives/osx_64/tor`
	`git update-index --chmod=+x tor/natives/osx_64/tor.real`
