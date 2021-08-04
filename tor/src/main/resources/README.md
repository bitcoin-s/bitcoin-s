# Updating Executables

1. Replace executables.
2. Make sure the Linux and the OSX binaries are executable:
	`git update-index --chmod=+x tor/natives/linux_64/tor`
	`git update-index --chmod=+x tor/natives/windows_64/tor.exe`
	`git update-index --chmod=+x tor/natives/osx_64/tor`
	`git update-index --chmod=+x tor/natives/osx_64/tor.real`
