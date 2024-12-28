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

          <button class="btn btn-sm btn-warning ml-2" @click="clearGetOutput">Clear</button>
        </div>

        <div v-if="getResult && Array.isArray(getResult) && getResult.length" class="mt-2">
          <div v-for="user in getResult" :key="user.id" class="alert alert-secondary" role="alert">
            <p>ID: {{ user.id }}</p>
            <p>Email: {{ user.email }}</p>
            <button class="btn btn-sm btn-info" @click="editUser(user)">Edit</button>
            <button class="btn btn-sm btn-danger" @click="deleteUser(user.id)">Delete</button>
          </div>
        </div>

        <div v-else class="alert alert-warning mt-2" role="alert">
          No users found.
        </div>

        <!-- Create User Form -->
        <div class="mt-4">
          <input v-model="newUser.email" type="email" class="form-control" placeholder="New User Email" />
          <button class="btn btn-sm btn-success mt-2" @click="createUser">Create User</button>
        </div>

        <!-- Edit User Form -->
        <div v-if="editingUser" class="mt-4">
          <input v-model="editingUser.email" type="email" class="form-control" placeholder="Edit User Email" />
          <button class="btn btn-sm btn-warning mt-2" @click="updateUser">Update User</button>
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
    data() {
        return {
            getResult: null,
            newUser: { email: '' },
            editingUser: null,
        };
    },
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
        async getDataById() {
            const id = this.$refs.get_id.value;
            if (!id) {
                console.error("ID is required");
                return;
            }

            try {
                const res = await fetch(`${USER_API_BASE_URL}/${id}`);
                if (!res.ok) {
                    const message = `An error has occurred: ${res.status} - ${res.statusText}`;
                    throw new Error(message);
                }

                const data = await res.json();
                this.getResult = this.formatResponse([data]);
            } catch (err) {
                console.error("Error in getDataById:", err);
                this.getResult = [];
            }
        },

        async createUser() {
            if (!this.newUser.email) {
                console.error("Email is required for creating a new user.");
                return;
            }

            try {
                const res = await fetch(USER_API_BASE_URL, {
                    method: 'POST',
                    headers: {'Content-Type': 'application/json'},
                    body: JSON.stringify(this.newUser),
                });

                if (!res.ok) {
                    const message = `An error has occurred: ${res.status} - ${res.statusText}`;
                    throw new Error(message);
                }

                this.newUser.email = ''; // Reset the form
                this.getAllData(); // Refresh the user list
            } catch (err) {
                console.error("Error in createUser:", err);
            }
        },

        async editUser(user) {
            this.editingUser = JSON.parse(JSON.stringify(user));
        },

        async updateUser() {
            if (!this.editingUser || !this.editingUser.email) {
                console.error("Email is required for updating the user.");
                return;
            }

            try {
                const res = await fetch(`${USER_API_BASE_URL}/${this.editingUser.id}`, {
                    method: 'PUT',
                    headers: {'Content-Type': 'application/json'},
                    body: JSON.stringify(this.editingUser),
                });

                if (!res.ok) {
                    const message = `An error has occurred: ${res.status} - ${res.statusText}`;
                    throw new Error(message);
                }

                this.editingUser = null; // Reset the form
                this.getAllData(); // Refresh the user list
            } catch (err) {
                console.error("Error in updateUser:", err);
            }
        },

        async deleteUser(id) {
            try {
                const res = await fetch(`${USER_API_BASE_URL}/${id}`, {method: 'DELETE'});

                if (!res.ok) {
                    const message = `An error has occurred: ${res.status} - ${res.statusText}`;
                    throw new Error(message);
                }

                this.getAllData();
            } catch (err) {
                console.error("Error in deleteUser:", err);
            }
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
