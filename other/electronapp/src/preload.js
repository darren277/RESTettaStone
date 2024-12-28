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

const { contextBridge } = require('electron');
console.log('Preload script loaded.');

try {
    contextBridge.exposeInMainWorld('api', {
        fetchData: async () => await fetchData(),
    });
} catch (error) {
    console.error('Error fetching data:', error);
}
