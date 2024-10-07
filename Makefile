include .env
PYTHON_BIN=venv/Scripts/

.PHONY: b f


docker-subnet:
	docker network create --subnet=$(SUBNET_CIDR) $(SUBNET_NAME)


docker-psql:
	docker run --name postgresql -e POSTGRES_USER=$(PG_USER) -e POSTGRES_PASSWORD=$(PG_PASS) --net $(SUBNET_NAME) --ip $(PG_IP) -p $(PG_PORT):$(PG_PORT) -v /data:/var/lib/postgresql/data -d postgres
	docker start postgresql

python-install:
	source $(PYTHON_BIN)activate && $(PYTHON_BIN)pip install -r requirements.txt

populate-db:
	source $(PYTHON_BIN)activate && $(PYTHON_BIN)python other/populate_pg_db.py


redis-build:
	cd other/redis && docker build -t redis:1 .

redis-run:
	docker run --name redis_container -d --net $(SUBNET_NAME) --ip $(REDIS_IP) --restart=always --publish $(REDIS_PORT):$(REDIS_PORT) redis:1

redis-set-variable:
	docker exec -it redis_container redis-cli set $(REDIS_KEY) $(REDIS_VALUE)


nginx-build:
	cd server/nginx && docker build --build-arg NGINX_VERSION=$(NGINX_VERSION) --build-arg LUA_JIT_VERSION=$(LUA_JIT_VERSION) --build-arg NGINX_PORT=$(NGINX_PORT) -t nginx:latest .

nginx-run:
	docker run -it --name nginx -p $(NGINX_PORT):$(NGINX_PORT) --env-file ".env" --env NGINX_PORT=$(NGINX_PORT) --net $(SUBNET_NAME) -v $(NGINX_DIR):/usr/share/nginx/html --ip $(NGINX_IP) -d nginx:latest


nginx-v2-build:
	cd server/nginx && docker build --build-arg NGINX_VERSION=$(NGINX_VERSION) --build-arg LUA_JIT_VERSION=$(LUA_JIT_VERSION) --build-arg ENTRYPOINT_VERSION=2 --build-arg NGINX_PORT=$(NGINX_PORT) --build-arg LOCATIONS="-more" -t nginx-v2:latest .

nginx-v2-run:
	docker run -it --name nginx-v2 -p $(NGINX_PORT):$(NGINX_PORT) --env-file ".env" --env ENTRYPOINT_VERSION=2 --env NGINX_PORT=$(NGINX_PORT) --env LOCATIONS="-more" --net $(SUBNET_NAME) -v $(NGINX_DIR):/usr/share/nginx/html --ip $(NGINX_IP) -d nginx-v2:latest


# Note: "-s -C ." are to suppress the "Entering directory" and "Leaving directory" messages.

# Backend: Runs commands in Makefile.backend
b:
	@$(MAKE) -s -C . -f Makefile.backend $(filter-out $@,$(MAKECMDGOALS))

# Frontend: Runs commands in Makefile.frontend
# NOTE FOR THE READER: There is no such file at the moment as the frontend applications have not been added yet.
f:
	@$(MAKE) -s -C . -f Makefile.f $(filter-out $@,$(MAKECMDGOALS))


# This is to avoid "No rule to make target" errors.
%:
	@:
