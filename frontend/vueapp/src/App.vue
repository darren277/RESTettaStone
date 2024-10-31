<template>
  <div id="app" class="container">
    <div class="card">
      <div class="card-header">Users</div>
      <div class="card-body">
        <div class="input-group input-group-sm">
          <button class="btn btn-sm btn-primary" @click="getAllData">Get All</button>

          <input type="text" ref="get_id" class="form-control ml-2" placeholder="Id" />
          <div class="input-group-append">
            <button class="btn btn-sm btn-primary" @click="getDataById">Get by Id</button>
          </div>

          <input type="text" ref="get_title" class="form-control ml-2" placeholder="Title" />
          <div class="input-group-append">
            <button class="btn btn-sm btn-primary" @click="getDataByTitle">Find By Title</button>
          </div>

          <button class="btn btn-sm btn-warning ml-2" @click="clearGetOutput">Clear</button>
        </div>

        <div v-if="getResult && Array.isArray(getResult) && getResult.length" class="mt-2">
          <div v-for="user in getResult" :key="user.id" class="alert alert-secondary" role="alert">
            <p>ID: {{ user.id }}</p>
            <p>Email: {{ user.email }}</p>
          </div>
        </div>

        <div v-else class="alert alert-warning mt-2" role="alert">
          No users found.
        </div>
      </div>
    </div>
  </div>

</template>


<script>
const NGINX_HOST = process.env.VUE_APP_NGINX_HOST;
const NGINX_PORT = process.env.VUE_APP_NGINX_PORT;

const FLASK_APP = `http://${NGINX_HOST}:${NGINX_PORT}/flaskapp`;

const USER_API_BASE_URL = `${FLASK_APP}/users`;


export default {
    name: 'app',
    data() {return {getResult: null}},
    methods: {
        formatName(user) {return `${user.firstName} ${user.lastName}`;},

        formatResponse(data) {
            if (!data || !Array.isArray(data)) {
                console.error("Unexpected data format:", data);
                return [];
            }

            return data.map(user => {
                return {
                    id: user.id,
                    email: user.email
                };
            });
        },

        async getAllData() {
            try {
                const res = await fetch(USER_API_BASE_URL);

                if (!res.ok) {
                  const message = `An error has occured: ${res.status} - ${res.statusText}`;
                  throw new Error(message);
                }

                const data = await res.json();

                this.getResult = this.formatResponse(data);
            } catch (err) {
                console.error("Error in getAllData:", err);
                this.getResult = [];
            }
        },
        getDataById() {
            console.log("getDataById called");
        },
        getDataByTitle() {
            console.log("getDataByTitle called");
        },
        clearGetOutput() {
            this.getResult = [];
            console.log("Output cleared");
        }
   }
}
</script>

<style lang="scss">
</style>
