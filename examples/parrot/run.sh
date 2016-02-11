#! /bin/bash

erlc parrot.erl
erl -pa ../../ebin ../../deps/*/ebin -eval "code:load_file(parrot)."

