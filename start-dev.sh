#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin \  -sname hiparser -s hiparser
