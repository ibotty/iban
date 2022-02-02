#! /usr/bin/env bash

usage () {
  cat << USAGE
Usage: $0 TASK

TASK is one of
- ghcid
USAGE

  exit 0
}

[ $# == 0 ] && usage

case $1 in
	ghcid)
		ghcid -c "cabal repl test:tests" -T "Main.main" --setup ":set args --hide-successes"
		;;
	*)
		usage
		;;
  esac