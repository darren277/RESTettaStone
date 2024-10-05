include .env
PYTHON_BIN=venv/Scripts/

PG_VARS=--env PG_USER=$(PG_USER) --env PG_PASS=$(PG_PASS) --env PG_DB=$(PG_DB) --env PG_HOST=$(PG_IP) --env PG_PORT=$(PG_PORT)
PG_VARS_BUILD=--build-arg PG_USER=$(PG_USER) --build-arg PG_PASS=$(PG_PASS) --build-arg PG_DB=$(PG_DB) --build-arg PG_HOST=$(PG_IP) --build-arg PG_PORT=$(PG_PORT)

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


aspnetapp-build:
	cd backend/aspnetapp && docker build --no-cache --build-arg ASPNETAPP_PORT=$(ASPNETAPP_PORT) $(PG_VARS_BUILD) -t aspnet_app:1 .

aspnetapp-run:
	cd backend/aspnetapp && docker run -d -it --rm --name aspnet_app_container --env ASPNETAPP_PORT=$(ASPNETAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(ASPNETAPP_IP) -p $(ASPNETAPP_PORT):$(ASPNETAPP_PORT) aspnet_app:1


springbootapp-build:
	cd backend/springbootapp && docker build --build-arg SPRINGBOOTAPP_PORT=$(SPRINGBOOTAPP_PORT) $(PG_VARS_BUILD) -t springboot_app:1 .

springbootapp-run:
	cd backend/springbootapp && docker run -d -it --rm --name springboot_app_container --env SPRINGBOOTAPP_PORT=$(SPRINGBOOTAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(SPRINGBOOTAPP_IP) -p $(SPRINGBOOTAPP_PORT):$(SPRINGBOOTAPP_PORT) springboot_app:1


railsapp-build:
	cd backend/railsapp && docker build --build-arg RAILSAPP_PORT=$(RAILSAPP_PORT) $(PG_VARS_BUILD) -t rails_app:1 .

railsapp-run:
	cd backend/railsapp && docker run -d -it --rm --name rails_app_container --env RAILSAPP_PORT=$(RAILSAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(RAILSAPP_IP) -p $(RAILSAPP_PORT):$(RAILSAPP_PORT) rails_app:1


perlapp-build:
	cd backend/perlapp && docker build --build-arg PERLAPP_PORT=$(PERLAPP_PORT) $(PG_VARS_BUILD) -t perl_app:1 .

perlapp-run:
	cd backend/perlapp && docker run -d -it --rm --name perl_app_container --env PERLAPP_PORT=$(PERLAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(PERLAPP_IP) -p $(PERLAPP_PORT):$(PERLAPP_PORT) perl_app:1


actixapp-build:
	cd backend/actixapp && docker build --build-arg ACTIXAPP_PORT=$(ACTIXAPP_PORT) $(PG_VARS_BUILD) -t actix_app:1 .

actixapp-run:
	cd backend/actixapp && docker run -d -it --rm --name actix_app_container --env ACTIXAPP_PORT=$(ACTIXAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(ACTIXAPP_IP) -p $(ACTIXAPP_PORT):$(ACTIXAPP_PORT) actix_app:1


swiftapp-build:
	cd backend/swiftapp && docker build --build-arg SWIFTAPP_PORT=$(SWIFTAPP_PORT) $(PG_VARS_BUILD) -t swift_app:1 .

swiftapp-run:
	cd backend/swiftapp && docker run -d -it --rm --name swift_app_container --env SWIFTAPP_PORT=$(SWIFTAPP_PORT) $(PG_VARS) --net $(SUBNET_NAME) --ip $(SWIFTAPP_IP) -p $(SWIFTAPP_PORT):$(SWIFTAPP_PORT) swift_app:1


goapp-build:
	cd backend/goapp && docker build --build-arg GOAPP_PORT=$(GOAPP_PORT) $(PG_VARS_BUILD) -t go_app:1 .

goapp-run:
	cd backend/goapp && docker run -d -it --rm --name go_app_container --env GOAPP_PORT=$(GOAPP_PORT) --net $(SUBNET_NAME) --ip $(GOAPP_IP) -p $(GOAPP_PORT):$(GOAPP_PORT) $(PG_VARS) go_app:1
