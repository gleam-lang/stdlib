cd "$(dirname "$0")"

cd ..

set -eu

gleam clean

gleam build

erl -pa ./build/dev/erlang/*/ebin -noshell -eval 'gleam@@main:run(benchee_runner)'
