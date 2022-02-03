#! /usr/bin/env bash

usage () {
  cat << USAGE
Usage: $0 <TASK>

TASK is one of
- dev: Open a ghcid that watches for changes and runs all tests.
- doc [PORT]: Open a hoogle development server. Uses port 3000 by default.
- repl: Launch a ghci session in the context where tests are run.
USAGE

  exit 0
}

[ $# == 0 ] && usage

case $1 in
	dev)
		ghcid -c "cabal repl test:tests" -T "Main.main" --setup ":set args --hide-successes"
		;;
	doc)
		hoogle server --local -p ${2:-3000} -n
		;;
	repl)
		cabal repl test:tests
		;;
	*)
		usage
		;;
  esac