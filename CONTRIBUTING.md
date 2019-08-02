First of all, many thanks for showing your interest in contributing to **memrise.el** and getting all the way to this guide!

There are many ways to get involved and help this project to grow: [ideas](#ideas), [documentation](#documentation), [issues](#issues), and of course, [the code](#development).

## Ideas

Brainstorming for ideas is a great way to contribute. How **memrise.el** can be more useful? More user-friendly? What features will make it much better? Please check ["feature idea"](https://github.com/SavchenkoValeriy/memrise.el/labels/feature%20idea) tag and add your own!

## Documentation

**memrise.el** is quite a big package (compared to average Emacs Lisp projects) and it has a lot of text: in-code docstrings and comments, [README](./README.md), and this file. Making it more clear and understandable should be an all-time goal. Any help here is highly appreciated.

## Issues

It is probably a good idea to keep [issues](https://github.com/SavchenkoValeriy/memrise.el/issues) organized and not getting out of control. Helping other users, answering [questions](https://github.com/SavchenkoValeriy/memrise.el/labels/question), reproducing bugs and simply suggesting labels for the issues is a great help.

### Debugging

It might be a good addition to a reproduced bug, to get to the root of it with debugging.

Debugging regular functions, widgets, and overall logic is pretty much straigthforward using [Edebug](https://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html#Edebug).

In many cases though, the problem comes from the outside, from the **Memrise** response. For these ocasions, you can use function `memrise-debug-connection`, which turns on **request.el** debug output showing `curl` commands, responses and so on. Another useful feature is saving JSON objects received from **Memrise**. You can turn on debug saving it into a separate `*memrise-result*` buffer by setting `memrise-save-received-data`:

``` emacs-lisp
(setq memrise-save-received-data t)
```

## Development

Here we are at the core of this guide. To start with the code it's probably easier to pick an issue ([bug](https://github.com/SavchenkoValeriy/memrise.el/labels/bug), [enhancement](https://github.com/SavchenkoValeriy/memrise.el/labels/enhancement), or [epic](https://github.com/SavchenkoValeriy/memrise.el/labels/epic) if you are brave) and dive in. Create an issue if you have a question about the package's internals (maybe we can use some sort of IRC later on). Below you can find some useful information about the project and its' dependencies.

### Tests

**memrise.el** uses [cask](https://github.com/cask/cask) for dependency management. After the installation of **cask**, install all dependencies and run tests:

```
$ cask install
$ cask exec ert-runner
```

Tests should pass on *all* supported versions of Emacs. If you have troubles with one particular version, you can work with it in your local environment using [evm](https://github.com/rejeep/evm).

### Dependencies

This section contains links to external libraries used extensively through **memrise.el**.

#### The Emacs Widget Library

**memrise.el** builds on the great foundation of [the Emacs Widget Library](https://www.gnu.org/software/emacs/manual/html_mono/widget.html). All of the session tests are implemented in terms of *widgets* and hack largely on some of their internals (especially the buttons). Reading through the linked manual is a good place to start. The majority of the **memrise.el** code dealing with *widgets* is gathered in [memrise-widget.el](./memrise-widget.el).

#### jeison

JSON objects served by **Memrise** API are far from trivial. Here are examples for [review](./assets/review.json) and [learn](./assets/learn.json) sessions. It used to be tons of boilerplate code just to go through those structures, and when **Memrise** changed it, I was completely discouraged for about a year.

As the result, I even implemented a separate library called [jeison](https://github.com/SavchenkoValeriy/jeison) to make it a bit easier to deal with in Emacs Lisp. *Jeison* does the most work to parse those JSON objects into **memrise.el** internal structures. All this is declared in [memrise-session-objects.el](./memrise-session-objects.el) with extra manual parsing in [memrise-session-parser.el](./memrise-session-parser.el).

#### Request.el

All the requests send to **Memrise** are managed by [Request.el](https://github.com/tkf/emacs-request).

#### dash.el

[dash.el](https://github.com/magnars/dash.el) is pretty much the most famous external library for Emacs Lisp. **memrise.el** uses different bits of it all around the code.
