backend: 
	cd erlang && rebar3 release
	cd ..
	erlang/_build/default/rel/dev/bin/dev daemon

backend-stop: backend
	erlang/_build/default/rel/dev/bin/dev stop

react-app:
	cd frontend/my-vue-app && npm run dev

app: backend react-app

clean-all:
	cd erlang && make clean
	cd ..
	cd frontend/my-vue-app && rm -rf dist

backend-build:
	cd erlang && rebar3 compile

backend-tests:
	cd erlang && rebar3 do eunit, ct
