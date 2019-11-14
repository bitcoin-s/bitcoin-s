## Ammonite

We use a tool called [ammmonite](https://ammonite.io/) that allows for Scala scripting. There are various ways to execute ammonite scripts, for now we will talk about how to run these scripts from bloop

### Executing an ammonite script


All of our ammonite scripts live in [`scripts/`](../scripts).

You can execute an ammonite script from `bloop` with the following command

```bash
✗ bloop run scripts --args scripts/{chain,wallet}/name-of-script.sc -- [arg1] [arg2] ... [argN]
```

You can also execute the ammonite script via `sbt` with the following command

```bash
sbt:bitcoin-s> scripts/test:run scripts/{chain,wallet}/name-of-script.sc [arg1] [arg2] ... [argN]
```

