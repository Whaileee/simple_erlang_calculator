.PHONY: app

backend:
	cd erlang && rebar3 release
	cd ..
	erlang/_build/default/rel/dev/bin/dev daemon

backend-stop:
	erlang/_build/default/rel/dev/bin/dev stop

react-app:
	cd frontend/my-vue-app && npm run dev

app: backend react-app
