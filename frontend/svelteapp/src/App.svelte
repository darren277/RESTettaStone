<script>
    import { onMount } from 'svelte';

    let users = [];
    let newUserEmail = '';
    let editUserId = null;
    let editUserEmail = '';

    const API_BASE_URL = 'http://localhost:8084/flaskapp';

    onMount(async () => {fetchUsers();});

    async function fetchUsers() {
        try {
            const res = await fetch(`${API_BASE_URL}/users`);
            users = await res.json();
        } catch (error) {
            console.error('Error fetching users:', error);
        }
    }

    async function createUser() {
        try {
            const res = await fetch(`${API_BASE_URL}/users`, {method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ email: newUserEmail })});
            const createdUser = await res.json();
            users = [...users, createdUser];
            newUserEmail = '';
        } catch (error) {
            console.error('Error creating user:', error);
        }
    }

    function startEdit(user) {
        editUserId = user.id;
        editUserEmail = user.email;
    }

    async function updateUser() {
        try {
            const res = await fetch(`${API_BASE_URL}/users/${editUserId}`, {method: 'PUT', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ email: editUserEmail })});
            const updatedUser = await res.json();
            users = users.map(user => {
                if (user.id === editUserId) {return updatedUser;}
                return user;
            });

            editUserId = null;
            editUserEmail = '';
        } catch (error) {
            console.error('Error updating user:', error);
        }
    }

    async function deleteUser(id) {
        try {
            const res = await fetch(`${API_BASE_URL}/users/${id}`, {method: 'DELETE'});
            if (res.ok) {users = users.filter(user => user.id !== id);}
        } catch (error) {
            console.error('Error deleting user:', error);
        }
    }
</script>

<style>
    .user-list {
        margin: 1rem 0;
    }

    .user-item {
        display: flex;
        align-items: center;
        margin-bottom: 0.5rem;
    }

    .user-item span {
        flex: 1;
    }

    .edit-form {
        margin-top: 1rem;
    }
</style>

<h1>Svelte User Management</h1>

<!-- CREATE USER -->
<div>
    <h2>Create User</h2>
    <input type="email" placeholder="New user email..." bind:value={newUserEmail} />
    <button on:click={createUser}>Create</button>
</div>

<!-- LIST / DELETE USERS -->
<div class="user-list">
    <h2>User List</h2>
    {#each users as user}
        <div class="user-item">
            <span>{user.email} (id: {user.id})</span>
            <button on:click={() => startEdit(user)}>Edit</button>
            <button on:click={() => deleteUser(user.id)}>Delete</button>
        </div>
    {/each}
</div>

<!-- EDIT USER -->
{#if editUserId}
    <div class="edit-form">
        <h2>Edit User (ID: {editUserId})</h2>
        <input type="email" placeholder="Edit user email..." bind:value={editUserEmail} />
        <button on:click={updateUser}>Update</button>
        <button on:click={() => {editUserId = null; editUserEmail = '';}}>Cancel</button>
    </div>
{/if}
