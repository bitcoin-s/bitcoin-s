---
id: windows-users
title: windows-users
---
This will be a guide directed to Windows Users hoping to contribute to Bitcoin-s and its development.
## Bloop
Reference the `contributing.md` document for a more descriptive guide on what bloop is and how to use it.
When following the installation guide to bloop on their website you will use `scoop` to install `bloop`. After installation
there will more than likely be issues with `bloop` looking for `.jar` files in a folder like `C:\root\.ivy2\`. The true installation
location of the `.ivy2` folder is likely in `C:\users\{your_username}\.ivy2\`. Once we have located our `.ivy2` folder 
we will want to direct `bloop` on how to find the files in the new location. I found it was quite simple to use Windows 
symbolic link (Note: if you are a running a Linux Subsystem this will also influence where your Linux bloop looks for this directory and thus will break as subsystems will have to use the `mnt` folder to access `C` drive). 
To create the symbolic link we run  
```mklink /D C:\root\.ivy2\ C:\users\{your_username}\.ivy2\```
the `/D` option specifies that it is a directory. You will need to run this command in `cmd.exe` instead of `Windows Powershell` as it is not a standalone executable.

## Running a Bitcoind node
Currently there are written changes in code to make it so you are run a node on Windows. Path specs are based on out of box installation. In the case you receive an error like `Could not locate bitcoind on user PATH`
then you will need to do some tweaking of either the code or moving your folder into the correct location. Currently this is specified by the `DEFAULT_DATADIR` in `BitcoindConfig` within `Bitcoind-rpc`. When you install bitcoin out of box there are 2 folders
created, 1 which is contained in the location on the wiki and that contains data generated after syncing. The current directory `DEFAULT_DATADIR` is pointing at which contains the `bitcoind.exe` file. 
That file is what is necessary to start up a bitcoind node. 

## Running Linux on a Windows Machine
If you are looking to develop in a Linux environment on a Windows Machine I have had success with [Windows Subsystem for Linux (WSL)](https://docs.microsoft.com/en-us/windows/wsl/install-win10).
If you are interested in working on both Linux and Windows for development reasons I would also recommend [Windows Terminal](https://github.com/microsoft/terminal) as a way to hold many different terminals all in one window including your 

Linux distro if you set up a WSL. With that being said the most important thing to consider when running Linux on a Windows Machine while also developing on Windows, there will be differences in setting up directories and pathing especially for `bloop`. I will update if I find a functional workaround.
Currently trying to get bloop working on a WSL that already has linked folders to have bloop work on Windows. Linux cannot recognize the file paths for directories as they are in Windows format, trying to find a workaround.
