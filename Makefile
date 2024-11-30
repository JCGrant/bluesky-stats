dev-app:
	find . -name "*.hs" | TARGET=app entr -r make run

run:
	if [ -z ${TARGET} ]; then echo "TARGET is not set"; exit 1; fi
	DB_CONNECTION_STRING="${DB_CONNECTION_STRING}" cabal run bluesky-stats-${TARGET}

docker-dev:
	docker compose -f docker-compose.dev.yml up

docker-build:
	docker build -t jcgrant/bluesky-stats .

docker-run:
	if [ -z ${TARGET} ]; then echo "TARGET is not set"; exit 1; fi
	docker run --network="host" -it jcgrant/bluesky-stats \
		-e TARGET="${TARGET}" \
		-e DB_CONNECTION_STRING="${DB_CONNECTION_STRING}"

docker-push:
	docker push jcgrant/bluesky-stats
