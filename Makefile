include .env
PYTHON_BIN=venv/Scripts/

PG_VARS=--env PG_USER=$(PG_USER) --env PG_PASS=$(PG_PASS) --env PG_DB=$(PG_DB) --env PG_HOST=$(PG_IP) --env PG_PORT=$(PG_PORT)

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
	cd server/nginx && docker build --build-arg NGINX_VERSION=$(NGINX_VERSION) --build-arg LUA_JIT_VERSION=$(LUA_JIT_VERSION) -t nginx:latest .

nginx-run:
	docker run -it --name nginx -p $(NGINX_PORT):$(NGINX_PORT) --env-file ".env" --net $(SUBNET_NAME) -v $(NGINX_DIR):/usr/share/nginx/html --ip $(NGINX_IP) -d nginx:latest


crowapp-build:
	cd backend/crowapp && docker build --build-arg CROWAPP_PORT=$(CROWAPP_PORT) -t crow_app:1 .

crowapp-run:
	cd backend/crowapp && docker run -d -it --rm --name crow_app_container --env CROWAPP_PORT=$(CROWAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(CROWAPP_IP) -p $(CROWAPP_PORT):$(CROWAPP_PORT) crow_app:1
