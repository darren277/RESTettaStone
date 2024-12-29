/**
 * This file will automatically be loaded by webpack and run in the "renderer" context.
 * To learn more about the differences between the "main" and the "renderer" context in
 * Electron, visit:
 *
 * https://electronjs.org/docs/tutorial/application-architecture#main-and-renderer-processes
 *
 * By default, Node.js integration in this file is disabled. When enabling Node.js integration
 * in a renderer process, please be aware of potential security implications. You can read
 * more about security risks here:
 *
 * https://electronjs.org/docs/tutorial/security
 *
 * To enable Node.js integration in this file, open up `main.js` and enable the `nodeIntegration`
 * flag:
 *
 * ```
 *  // Create the browser window.
 *  mainWindow = new BrowserWindow({
 *    width: 800,
 *    height: 600,
 *    webPreferences: {
 *      nodeIntegration: true
 *    }
 *  });
 * ```
 */

import './index.css';

console.log('👋 This message is being logged by "renderer.js", included via webpack');

async function populateTable() {
    const data = await window.api.fetchData(); // Fetch data using the exposed API

    console.log('Data:', data);
  
    const table = document.createElement('table');
    table.border = '1';
    table.width = '100%';
    table.cellPadding = '5';
    table.cellSpacing = '0';
  
    const thead = document.createElement('thead');
    const tbody = document.createElement('tbody');
  
    const headerRow = document.createElement('tr');
    ['ID', 'Email', 'Actions'].forEach((header) => {
        const th = document.createElement('th');
        th.appendChild(document.createTextNode(header));
        headerRow.appendChild(th);
    });
    thead.appendChild(headerRow);

    data.forEach((user) => {
        const row = document.createElement('tr');

        const idCell = document.createElement('td');
        idCell.appendChild(document.createTextNode(user.id));
        row.appendChild(idCell);

        const emailCell = document.createElement('td');
        const emailInput = document.createElement('input');
        emailInput.type = 'text';
        emailInput.value = user.email;
        emailCell.appendChild(emailInput);
        row.appendChild(emailCell);

        const actionsCell = document.createElement('td');
        const updateButton = document.createElement('button');
        updateButton.textContent = 'Update';
        updateButton.onclick = async () => {
            try {
                await window.api.updateUser(user.id, emailInput.value);
                alert('User updated successfully.');
            } catch (err) {
                console.error(err);
                alert('Failed to update user.');
            }
        };
        actionsCell.appendChild(updateButton);

        const deleteButton = document.createElement('button');
        deleteButton.textContent = 'Delete';
        deleteButton.onclick = async () => {
            try {
                await window.api.deleteUser(user.id);
                row.remove();
                alert('User deleted successfully.');
            } catch (err) {
                console.error(err);
                alert('Failed to delete user.');
            }
        };
        actionsCell.appendChild(deleteButton);
        row.appendChild(actionsCell);

        tbody.appendChild(row);
    });

    table.appendChild(thead);
    table.appendChild(tbody);

    const container = document.getElementById('table-container');
    container.innerHTML = '';
    container.appendChild(table);
}

async function handleCreateUser() {
    const emailInput = document.getElementById('new-user-email');
    const email = emailInput.value;
    if (!email) {
        alert('Email is required.');
        return;
    }
    try {
        await window.api.createUser(email);
        emailInput.value = '';
        await populateTable();
        alert('User created successfully.');
    } catch (err) {
        console.error(err);
        alert('Failed to create user.');
    }
}

document.getElementById('create-user-btn').addEventListener('click', handleCreateUser);

// Call the function when the content is loaded
window.addEventListener('DOMContentLoaded', () => {
    populateTable();
});

