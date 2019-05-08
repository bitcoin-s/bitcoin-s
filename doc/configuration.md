# bitcoin-s configuration

bitcoin-s uses [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md)
to configure various parts of the application the library offers. HOCON is a
superset of JSON, that is, all valid JSON is valid HOCON.

All configuration for bitcoin-s is under the `bitcoin-s` key. The most interesting
configurable parts right now are `datadir` and `network`. See
[`db-commons/src/main/resources/reference.conf`](../db-commons/src/main/resources/reference.conf)
for more information. In the future there will be separate keys under `bitcoin-s`
for the `wallet`, `chain` and `node` modules.

If you have a file `application.conf` anywhere on your classpath when using
bitcoin-s, the values there take precedence over the ones found in our
`reference.conf`.

## Internal configuration

Database connections are also configured by using HOCON. This is done in
[`db.conf`](../db-commons/src/main/resources/db.conf)
(as well as [`application.conf`](../testkit/src/main/resources/application.conf)
in `testkit` for running tests). The options exposed here are **not** intended to
be used by users of bitcoin-s, and are internal only.
