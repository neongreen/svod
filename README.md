# Svod (Свод)

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/svod-music/svod.svg?branch=master)](https://travis-ci.org/svod-music/svod)

*Work in progress. If you have questions about the project, please send me
 an email to markkarpov at opmbx dot org.*

* [Overview](#overview)
* [Learn more about the Svod project](#learn-more-about-the-svod-project)
* [Svod packages](#svod-packages)
* [Documentation](#documentation)
* [Running Svod web app locally](#runnig-svod-web-app-locally)
* [Deploying](#deploying)
* [Contribution](#contribution)
* [License](#license)

This is an open source web application of Svod (Russian: Свод) netlabel
written in Haskell using Yesod web framework.

Although the project is of rather local nature, I nevertheless decided to
write technical descriptions and documentation in English for the following
reasons:

* Some people who do not speak Russian may want to read source code or play
  with the project.

* All Russian developers speak English (or will speak it when they get good
  enough, it's lingua franca anyway, especially in IT).

* I find it difficult to translate all tech terms into Russian and they end
  up sounding extremely nerdy. Well, maybe they are nerdy in English too,
  but…

Presentations, conceptual articles, etc. are on the other hand in Russian
only and there are no plans to translate them.

## Overview

The Svod web application is built to allow (semi-) automatic publication of
audio productions under permissive licenses, such as various flavors of
[Creative Commons](https://creativecommons.org/). The Svod project only
distributes [FLAC](https://xiph.org/flac/) audio and there is no attempt to
make audience pay for it, i.e. it's also free as in “free beer”. Artists can
ask for donations on their profile pages, but it's up to them completely.

In Russia, most part of audience is not willing to pay for music — that's a
fact of today's life. We can make profit only through live performances and
via ads (though no ads will be allowed on the main site). The service is
therefore exists solely to increase visibility of particular artists by
acting as a filter for enormous amount of often amateur content. The filter
is opinionated, but there is no way to avoid this from my point of view.

For artists, the service provides free web storage for their works and
certain popularity boost. This is the first level of Svod — artist
relationship. When artist's music achieves certain degree of success as
measured by our team, we can talk about the second level that provides
profit for Svod and for artist as well if he/she wishes to participate on
that level. This includes some management (possibly organization of events
and concerts) and streaming of their music via services that earn money via
selling (targeted) ads.

This repository contains the web application. Main purpose of the
application is to allow any person send us a demo or finished production,
and to allow us examine it and publish it, so users can access it.

The issue tracker can be used for technical issues as well as any other
feedback regarding the service.

## Learn more about the Svod project

If you want to learn more about the project, check out these repos:

* [Collection of presentations about Svod](https://github.com/svod-music/presentations)
* [Conceptual articles as they appear on the site](https://github.com/svod-music/svod-concepts)

You can also open pull requests and actually change the information. Please
think carefully before doing this, but the fact is: we're open to changes
and anyone can propose ideas.

## Svod packages

The Svod web app uses the following packages/resources:

* [Svod core (database and file management abstracted from web)](https://github.com/svod-music/svod-core)
* [Search query mini-language](https://github.com/svod-music/svod-search-query)
* [Linter that prevents some suspicious content](https://github.com/svod-music/svod-lint)
* [Database population app for development purposes](https://github.com/svod-music/svod-devel)
* [Twitter intergration](https://github.com/svod-music/twitter-integration)
* [Logo](https://github.com/svod-music/svod-logo)

## Documentation

All components are fully documented, but you will need to generate Haddock
documentation yourself locally.

## Running Svod web app locally

This section describes how to set up Svod web app locally and actually play
with it. The application is split into several relatively independent
packages/repos, so it may be not entirely obvious how to make it work. All
instructions here are for Unix-like systems, such as Linux.

First, we need to install PostgreSQL. You should find out how to do this
from repositories of your distro. Arch Linux users can execute this:

```
# pacman -S postgresql
```

We will need a new user (role) and database. By default we use `svod` as
user name and database name, you can choose something different, of course.
In this case edit `config/settings.yml` file and change `user` (`password`)
and `database` attributes of `database` object. Defaults look like this:

```yaml
database:
  user:     "_env:PGUSER:svod"
  password: "_env:PGPASS:svod"
  host:     "_env:PGHOST:localhost"
  port:     "_env:PGPORT:5432"
  database: "_env:PGDATABASE:svod"
  poolsize: "_env:PGPOOLSIZE:10"
```

To create user `svod` and database `svod` (if you have `sudo`):

```
$ sudo -i -u postgres
[postgres]$ initdb --locale $LANG -E UTF8 -D '/var/lib/postgres/data'
[postgres]$ createuser -d svod
[postgres]$ createdb svod -U svod
[postgres]$ exit
```

Now you should have user named `svod` and a database with the same name.

Now we need to clone the repo and build the application.

TODO Finish this section

## Deploying

TODO

## Contribution

Contributions are very welcome. Do not hesitate to open an issue (even if
it's not technical) or a PR.

## License

Copyright © 2015–2016 Mark Karpov

Distributed under GNU GPL, version 3.
