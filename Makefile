repository=102103889482.dkr.ecr.us-east-1.amazonaws.com/simple-tex-service
tag=$(shell git rev-parse HEAD)
image=$(repository):$(tag)

deploy: FORCE
	make build-deb
	make build-image
	make push-image
	make rancher-up

rancher-up: FORCE
	image=$(image) rancher up --force-upgrade

up:
	image=$(image) docker-compose up

build-deb:
	rm -rf ./dist/*
	./stack-fpm

build-image: FORCE
	docker build --build-arg package=./dist/$(shell ls ./dist | grep deb) -t $(image) .
	docker tag $(image) $(repository):latest

push-image: FORCE
	docker push $(image)
	docker push $(repository):latest
	docker tag $(image) beijaflorio/tex.beijaflor.io:$(tag)
	docker tag $(image) beijaflorio/tex.beijaflor.io:latest
	docker push beijaflorio/tex.beijaflor.io:$(tag)
	docker push beijaflorio/tex.beijaflor.io:latest

FORCE:
