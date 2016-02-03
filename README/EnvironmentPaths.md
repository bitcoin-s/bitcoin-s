###Setting daemon to your path in environmental settings
In order for this library to properly function, we need to edit your PATH in your environmental variables.

####Windows
1. Open your **Control Panel**. In the search box, search "environment variable". Click **Edit the system environment variables**.
2. Your system properies will open. Go to the **Advanced** tab, click **Environment Variables...** at the bottom.
3. There will be two boxes: Users variables and System variables. In the user variables (top box), find the PATH variable, highlight it, and click **Edit**.
4. We need the path to the daemon subfolder in your Bitcoin folder. Find your path and click **New** in the Edit Environment Variable window. Enter the path to your daemon subfolder. (By default, Bitcoin will be placed in your Program Files. The PATH will then be C:\Program Files\Bitcoin\daemon)
5. Click **OK** three times to save your changes. 
6. To test, open a **Command Prompt**. Enter `start /b bitcoind -testnet`. 
7. Wait a few seconds and enter `bitcoin-cli -testnet getblockcount`. If it returns a block count, everything is working properly. Stop your test server with `bitcoin-cli -testnet stop`.

If you get an error message, backtrack and make sure the steps were completed as followed. You can also refer to: https://www.java.com/en/download/help/path.xml
