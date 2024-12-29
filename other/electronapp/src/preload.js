// See the Electron documentation for details on how to use preload scripts:
// https://www.electronjs.org/docs/latest/tutorial/process-model#preload-scripts

const API_URL = 'http://127.0.0.1:8084/flaskapp/users';

async function fetchData() {
    const response = await fetch(API_URL,
       {
           headers: {
               //Authorization: "token {Personal_Access_Token}",
           },
       }
    );

    if (!response.ok) {
        const message = `An error has occured: ${response.status}`;
        console.error(message);
        throw new Error(message);
    }

    const data = await response.json();

    return data;
};

async function createUser(email) {
    const response = await fetch(API_URL, {method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ email })});
    if (!response.ok) throw new Error(`Error: ${response.status}`);
    return await response.json();
}

async function updateUser(id, email) {
    const response = await fetch(`${API_URL}/${id}`, {method: 'PUT', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ email })});
    if (!response.ok) throw new Error(`Error: ${response.status}`);
    return await response.json();
}

async function deleteUser(id) {
    const response = await fetch(`${API_URL}/${id}`, { method: 'DELETE' });
    if (!response.ok) throw new Error(`Error: ${response.status}`);
    return true;
}

const { contextBridge } = require('electron');
console.log('Preload script loaded.');

try {
    contextBridge.exposeInMainWorld('api', {
        //fetchData: async () => await fetchData(),
        fetchData,
        createUser,
        updateUser,
        deleteUser
    });
} catch (error) {
    console.error('Error fetching data:', error);
}
