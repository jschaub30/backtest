
help :
	@echo "------------------------------------------"
	@echo "Look in README.md for general information "
	@echo "------------------------------------------"
	@echo
	@echo "build       ... build docker image        "
	@echo "run         ... run docker image          "
	@echo
	@echo "help	       ... print this message        "
	@echo "------------------------------------------"


build:
	docker build -t r-backtest .

run:
	docker run --rm -v`pwd`:/code r-backtest:latest

push:
	@echo Pushing images to ${REGISTRY}
	docker push ${REGISTRY}/omnia/${PROJECT}:${VERSION}
