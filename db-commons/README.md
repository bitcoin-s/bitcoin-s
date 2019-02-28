### db-commons

This is a project that is meant to contain re-usable database related infrastructure for bitcoin-s. This project is a dependency of [`wallet`](../wallet/) and [`node`](../node). 

The library that bitcoin-s currently uses for database related things is called [`Slick`](http://slick.lightbend.com/doc/3.3.0/).

The most important file in this project is [`DbConfig`](src/main/scala/org/bitcoins/db/DbConfig.scala). This provides a
common way for databases to be accessed from configuration files. For more information on how Slick configuration files
work please see this [reference](http://slick.lightbend.com/doc/3.3.0/gettingstarted.html#database-configuration).


This project expects the following keys for databases

- mainnetDb
- testnet3Db
- regtestDb
- unittestDb

This will be read by [`DbConfig`](src/main/scala/org/bitcoins/db/DbConfig.scala) to specify database information related
to a specific project. You can look at the database configuration for the [`node`](../node/src/main/resources/application.conf) project for an example
of how this works.